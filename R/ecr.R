#' @title
#' Interface to \pkg{ecr} similar to the \code{\link[stats]{optim}} function.
#'
#' @description
#' The most flexible way to setup evolutionary algorithms with \pkg{ecr2} is by
#' explicitely writing the evolutionary loop utilizing various ecr2 helper functions
#' However, in everyday
#' life R users frequently need to optimize a single-objective R function.
#' The \code{ecr} function thus provides a more R like interface for single
#' objective optimization similar to the interface of the \code{\link[stats]{optim}}
#' function.
#'
#' @keywords optimize
#'
#' @seealso \code{\link{initECRControl}} for building the control object,
#' \code{\link{makeOptimizationTask}} to define an optimization task.
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
#' @template arg_custom_constants
#' @template arg_logger
#' @template arg_monitor
#' @template arg_more_args
#' @template arg_initial_solutions
#' @template arg_parent_selector
#' @template arg_survival_selector
#' @template arg_generator
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_terminators
#' @return [\code{\link{ecr_result}}]
#' @examples
#' fn = function(x) {
#'    sum(x^2)
#'  }
#'
#' res = ecr(fn, n.dim = 2L, n.objectives = 1L, lower = c(-5, -5), upper = c(5, 5),
#'  representation = "float", mu = 20L, lambda = 10L)
#' @export
ecr = function(
  fitness.fun, minimize = NULL, n.objectives = NULL,
  n.dim = NULL, lower = NULL, upper = NULL, n.bits,
  representation, mu, lambda, perm = NULL,
  p.recomb = 0.7, p.mut = 0.3,
  survival.strategy = "plus", n.elite = 0L,
  custom.constants = list(), logger = NULL, monitor = NULL,
  more.args = list(), initial.solutions = NULL,
  parent.selector = NULL,
  survival.selector = NULL,
  generator = NULL,
  mutator = NULL,
  recombinator = NULL,
  terminators = list(stopOnIters(100L))) {

  if (!isSmoofFunction(fitness.fun)) {
    n.objectives = asInt(n.objectives, lower = 1L)
    if (is.null(minimize))
      minimize = rep(TRUE, n.objectives)
  }
  assertChoice(representation, c("binary", "float", "permutation", "custom"))
  assertChoice(survival.strategy, c("comma", "plus"))
  assertNumber(p.recomb, lower = 0, upper = 1)
  assertNumber(p.mut, lower = 0, upper = 1)
  assertList(terminators, any.missing = FALSE, all.missing = FALSE, types = "ecr2_terminator")
  mu = asInt(mu, lower = 1L)
  lambda.lower = if (survival.strategy == "plus") 1L else mu
  lambda = asInt(lambda, lower = lambda.lower)

  control = if (representation == "binary") {
    initECRControlBinary(fitness.fun, n.bits = n.bits, n.objectives = n.objectives, minimize = minimize)
  } else if (representation == "float") {
    initECRControlFloat(fitness.fun, lower = lower, upper = upper, n.dim = n.dim,
      n.objectives = n.objectives, minimize = minimize)
  } else if (representation == "permutation") {
    initECRControlPermutation(fitness.fun, perm = perm,
      n.objectives = n.objectives, minimize = minimize)
  }

  n.objectives = control$task$n.objectives

  control = registerMutator(control, coalesce(mutator, getDefaultEvolutionaryOperators(representation, "mutator", n.objectives, control)))
  control = registerRecombinator(control, coalesce(recombinator, getDefaultEvolutionaryOperators(representation, "recombinator", n.objectives, control)))
  control = registerGenerator(control, coalesce(generator, getDefaultEvolutionaryOperators(representation, "generator", n.objectives, control)))
  control = registerSurvivalSelector(control, coalesce(survival.selector, getDefaultEvolutionaryOperators(representation, "survival.selector", n.objectives, control)))
  control = registerMatingSelector(control, coalesce(parent.selector, getDefaultEvolutionaryOperators(representation, "parent.selector", n.objectives, control)))
  # control = registerLogger(control, logger = setupECRDefaultLogger(
  #   log.stats = list("min", "max", "mean"),#, "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
  #   log.pop = TRUE, init.size = 1000L)
  # )

  # init logger
  #FIXME: logger params should be passable to ecr -> logger.pars
  log = initLogger(log.stats = list("min", "max", "mean"),#, "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
    log.pop = TRUE, init.size = 1000L)

  # simply pass stuff down to control object constructor
  population = initPopulation(mu = mu, control = control, initial.solutions = initial.solutions)
  fitness = evaluateFitness(population, control)

  repeat {
    # generate offspring
    offspring = generateOffspring(control, population, fitness, lambda = lambda, p.recomb = p.recomb, p.mut = p.mut)
    fitness.offspring = evaluateFitness(offspring, control)

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
  return(setupResult.ecr_multi_objective(population, fitness, control, log, stop.object, ...))
}
