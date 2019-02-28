#' @title
#' Computes the fitness value(s) for each individual of a given set.
#'
#' @description
#' This function expects a list of individuals, computes the fitness and always
#' returns a matrix of fitness values; even in single-objective optimization a
#' (1 x n) matrix is returned for consistency, where n is the number of individuals.
#' This function makes use of \code{\link[parallelMap]{parallelMap}} to
#' parallelize the fitness evaluation.
#'
#' @template arg_control
#' @param inds [\code{list}]\cr
#'   List of individuals.
#' @param ... [any]\cr
#'   Optional parameters passed down to fitness function.
#' @return [\code{matrix}].
#' @export
evaluateFitness = function(control, inds, ...) {
  assertList(inds)
  assertClass(control, "ecr_control")
  fitness.fun = control$task$fitness
  if (is.null(fitness.fun))
    stopf("Fitness function not found in control object. Did you use initECRControl*
      to generate the control object?")
  if (isSmoofFunction(fitness.fun)) {
    if (isVectorized(fitness.fun)) {
      fitness = do.call(fitness.fun, c(list(do.call(cbind, inds)), ...))
    } else {
      fitness = do.call(cbind, parallelLapply(inds, fitness.fun, ..., level = "ecr.evaluateFitness"))
    }
    if (!is.matrix(fitness))
      fitness = matrix(fitness, nrow = 1L)
    fitness = makeFitnessMatrix(fitness, control)
    return(fitness)
  }
  fitness = parallelMap(function(x) do.call(fitness.fun, c(list(x), list(...))),
    inds, level = "ecr.evaluateFitness")
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  fitness = makeFitnessMatrix(fitness, control)
  return(fitness)
}
