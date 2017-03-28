#' @title (mu + lambda) selection
#'
#' @description Takes a population of mu individuals and another set of lambda
#' offspring individuals and selects mu individuals out of the union set according
#' to the survival selection strategy stored in the control object.
#'
#' @template arg_control
#' @param population [\code{list}]\cr
#'   Current set of individuals.
#' @param offspring [\code{list}]\cr
#'   Another set of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Matrix of fitness values for the individuals from \code{population}.
#'   This is only optional in the case that each individual in \code{population} has
#'   an attribute \dQuote{fitness}.
#' @param fitness.offspring [\code{matrix}]\cr
#'   Matrix of fitness values for the individuals from \code{offspring}.
#'   This is only optional in the case that each individual in \code{offspring} has
#'   an attribute \dQuote{fitness}.
#' @template arg_n_elite
#' @return [\code{list}] List with selected population and corresponding fitness matrix.
#' @rdname replace
#' @name replace
#' @export
replaceMuPlusLambda = function(control, population, offspring, fitness = NULL, fitness.offspring = NULL) {
  assertList(population)
  assertList(offspring)
  mu = length(population)

  if (is.null(fitness))
    fitness = do.call(cbind, lapply(population, function(ind) attr(ind, "fitness")))
  if (is.null(fitness.offspring))
    fitness.offspring = do.call(cbind, lapply(offspring, function(ind) attr(ind, "fitness")))

  assertMatrix(fitness, ncols = length(population))
  assertMatrix(fitness.offspring, ncols = length(offspring))

  # merge both, i.e., mu + lambda
  merged.pop = c(population, offspring)
  merged.fit = cbind(fitness, fitness.offspring)

  # now select mu from mu + lambda idnividuals
  surv.idx = selectForSurvival(control, merged.fit, n.select = mu)
  #surv.idx = control$selectForSurvival(merged.fit, n.select = mu)

  fitness = merged.fit[, surv.idx, drop = FALSE]
  fitness = makeFitnessMatrix(fitness, control)

  return(list(
    population = merged.pop[surv.idx],
    fitness = fitness
  ))
}

#' @rdname replace
#' @export
replaceMuCommaLambda = function(control, population, offspring, fitness = NULL, fitness.offspring = NULL, n.elite = base::max(ceiling(length(population * 0.1)), 1L)) {
  assertList(population)
  assertList(offspring)
  mu = length(population)

  if (is.null(fitness))
    fitness = do.call(cbind, lapply(population, function(ind) attr(ind, "fitness")))
  if (is.null(fitness.offspring))
    fitness.offspring = do.call(cbind, lapply(offspring, function(ind) attr(ind, "fitness")))

  assertMatrix(fitness, ncols = length(population))
  assertMatrix(fitness.offspring, ncols = length(offspring))
  n.elite = asInt(n.elite, lower = 0)
  # get elite individuals from current population
  #FIXME: only if we are not multi-objective
  surv = vector("list", mu)
  surv.fit = fitness
  if (n.elite > 0) {
    elite.idx = order(as.numeric(fitness), decreasing = !control$task$minimize)[1:n.elite]
    surv[1:n.elite] = population[elite.idx]
    surv.fit[, 1:n.elite] = fitness[, elite.idx, drop = FALSE]
  }
  # now get the remaining individuals from offspring
  n.select = mu - n.elite
  sel.idx = selectForSurvival(control, fitness.offspring, n.select = n.select)
  #sel.idx = control$selectForSurvival(fitness.offspring, n.select = n.select)
  surv[(n.elite + 1L):mu] = offspring[sel.idx]
  surv.fit[, (n.elite + 1L):mu] = fitness.offspring[, sel.idx, drop = FALSE]

  return(list(
    population = surv,
    fitness = makeFitnessMatrix(surv.fit, control = control)
  ))
}
