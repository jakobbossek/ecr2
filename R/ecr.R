#' @title
#' Interface to ecr similar to the optim function.
#'
#' @description
#' The most flexible way to setup evolutionary algorithms with ecr is by
#' explicitely writing the evolutionary loop utilizing various ecr utlity functions.
#' However, in everyday life R users frequently need to optimize a single-objective R function.
#' The \code{ecr} function thus provides a more R like interface for single
#' objective optimization similar to the interface of the \code{\link[stats]{optim}}
#' function.
#'
#' @keywords optimize
#'
#' @template arg_fitness_fun
#' @template arg_minimize
#' @template arg_n_objectives
#' @template arg_n_dim
#' @template arg_lower
#' @template arg_upper
#' @template arg_n_bits
#' @template arg_representation
#' @template arg_mu
#' @template arg_lambda
#' @template arg_perm
#' @template arg_p_recomb
#' @template arg_p_mut
#' @template arg_survival_strategy
#' @template arg_n_elite
#' @template arg_logstats
#' @template arg_logpop
#' @template arg_monitor
#' @template arg_initial_solutions
#' @template arg_parent_selector
#' @template arg_survival_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_terminators
#' @param ... [any]\cr
#'   Further arguments passed down to \code{fitness.fun}.
#' @return [\code{\link{ecr_result}}]
#' @examples
#' fn = function(x) {
#'    sum(x^2)
#' }
#' lower = c(-5, -5); upper = c(5, 5)
#' res = ecr(fn, n.dim = 2L, n.objectives = 1L, lower = lower, upper = lower,
#'  representation = "float", mu = 20L, lambda = 10L,
#'   mutator = setup(mutGauss, lower = lower, upper = upper))
#' @export
ecr = function(
  fitness.fun, minimize = NULL, n.objectives = NULL,
  n.dim = NULL, lower = NULL, upper = NULL, n.bits,
  representation, mu, lambda, perm = NULL,
  p.recomb = 0.7, p.mut = 0.3,
  survival.strategy = "plus", n.elite = 0L,
  log.stats = list(fitness = list("min", "mean", "max")),
  log.pop = FALSE,
  monitor = NULL,
  initial.solutions = NULL,
  parent.selector = NULL,
  survival.selector = NULL,
  mutator = NULL,
  recombinator = NULL,
  terminators = list(stopOnIters(100L)),
  ...) {

  if (!isSmoofFunction(fitness.fun)) {
    n.objectives = asInt(n.objectives, lower = 1L)
    if (is.null(minimize))
      minimize = rep(TRUE, n.objectives)
  }

  if (isSmoofFunction(fitness.fun)) {
    n.objectives = getNumberOfObjectives(fitness.fun)
    n.dim = getNumberOfParameters(fitness.fun)
    par.set = getParamSet(fitness.fun)
    upper = getUpper(par.set)
    lower = getLower(par.set)
  }

  assertChoice(representation, c("binary", "float", "permutation", "custom"))
  assertChoice(survival.strategy, c("comma", "plus"))
  assertNumber(p.recomb, lower = 0, upper = 1)
  assertNumber(p.mut, lower = 0, upper = 1)
  assertFlag(log.pop)
  assertList(terminators, any.missing = FALSE, all.missing = FALSE, types = "ecr_terminator")
  mu = asInt(mu, lower = 1L)
  lambda.lower = if (survival.strategy == "plus") 1L else mu
  lambda = asInt(lambda, lower = lambda.lower)

  control = initECRControl(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  #FIXME: ugly! get rid of the following line
  control$type = representation

  n.objectives = control$task$n.objectives

  if (representation != "custom" | !is.null(mutator))
    control = registerECROperator(control, "mutate", coalesce(mutator, getDefaultEvolutionaryOperators(representation, "mutator", n.objectives, control)))
  if (representation != "custom" | !is.null(recombinator))
    control = registerECROperator(control, "recombine", coalesce(recombinator, getDefaultEvolutionaryOperators(representation, "recombinator", n.objectives, control)))
  control = registerECROperator(control, "selectForSurvival", coalesce(survival.selector, getDefaultEvolutionaryOperators(representation, "survival.selector", n.objectives, control)))
  control = registerECROperator(control, "selectForMating", coalesce(parent.selector, getDefaultEvolutionaryOperators(representation, "parent.selector", n.objectives, control)))

  # init logger
  log = initLogger(control,
    log.stats = log.stats,
    log.pop = log.pop, init.size = 1000L)

  # generate population (depends on representation)
  gen.fun = NULL
  gen.pars = list()
  if (representation == "binary") {
    gen.fun = genBin; gen.pars = list(n.dim = n.bits)
  } else if (representation == "float") {
    gen.fun = genReal; gen.pars = list(n.dim = n.dim, lower = lower, upper = upper)
  } else if (representation == "permutation") {
    gen.fun = genPerm; gen.pars = list(n.dim = perm)
  } else {
    if (!is.null(initial.solutions)) {
      if (length(initial.solutions) != mu) {
        stopf("For custom representations the number of initial solutions need to be equal to mu.")
      }
    } else {
      stopf("For custom representations intial solutions need to be passed.")
    }
  }

  population = initial.solutions
  if (representation != "custom")
    population = do.call(initPopulation, c(list(mu = mu, gen.fun = gen.fun, initial.solutions = initial.solutions), gen.pars))
  fitness = evaluateFitness(control, population, ...)

  for (i in seq_along(population)) {
    attr(population[[i]], "fitness") = fitness[, i]
  }

  updateLogger(log, population, fitness = fitness, n.evals = mu)

  repeat {
    # generate offspring
    offspring = generateOffspring(control, population, fitness, lambda = lambda, p.recomb = p.recomb, p.mut = p.mut)
    fitness.offspring = evaluateFitness(control, offspring, ...)
    for (i in seq_along(offspring)) {
      attr(offspring[[i]], "fitness") = fitness.offspring[, i]
    }

    sel = if (survival.strategy == "plus") {
      replaceMuPlusLambda(control, population, offspring, fitness, fitness.offspring)
    } else {
      replaceMuCommaLambda(control, population, offspring, fitness, fitness.offspring, n.elite = n.elite)
    }

    population = sel$population
    fitness = sel$fitness

    # do some logging
    updateLogger(log, population, fitness, n.evals = lambda)

    stop.object = doTerminate(log, terminators)
    if (length(stop.object) > 0L)
      break
  }
  return(makeECRResult(control, log, population, fitness, stop.object))
}

makeECRResult = function(control, log, population, fitness, stop.object, ...) {
  n.objectives = control$task$n.objectives
  if (n.objectives == 1L)
    return(setupResult.ecr_single_objective(population, fitness, control, log, stop.object, ...))
  moo.res = setupResult.ecr_multi_objective(population, fitness, control, log, stop.object, ...)
  moo.res = filterDuplicated(moo.res)
  return(moo.res)
}
