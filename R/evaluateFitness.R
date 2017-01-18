#' @title
#' Computes the fitness values for each member of a given population.
#'
#' @description
#' This function expects a population, computes the fitness and
#' returns the matrix of fitness values. This function makes use of
#' \code{\link[parallelMap]{parallelMap}} to allow parallelization of fitness
#' evaluation.
#' Keep in mind, that the return value is a 1xn matrix in case of single-objective
#' optimization.
#'
#' @param population [\code{list}]\cr
#'   Population.
#' @param fitness.fun [\code{function}]\cr
#'   Fitness function.
#' @param task [\code{ecr_optimization_task}]\cr
#'   Optimization task.
#' @param control [\code{ecr_control}]\cr
#'   Control object containing all operators and further parameters.
#'   See \code{\link{setupECRControl}} and \code{\link{setupEvolutionaryOperators}}.
#' @return [\code{matrix}].
#' @export
evaluateFitness = function(population, fitness.fun, task, control) {
  # first check if objective/fitness function accepts lists/vectors
  if (control$vectorized.evaluation) {
    if (isSmoofFunction(fitness.fun)) {
      if (!isVectorized(fitness.fun)) {
        stopf("Vectorized evaluation of fitness function is activated, but fitness
          function '%s' is not vectorized.", getName(fitness.fun))
      }
      # vectorized smoof function are continuous only and expect a matrix input,
      # where each column is one input vector
      fitness = do.call(fitness.fun, c(list(do.call(cbind, population$individuals)), task$more.args))
    } else {
      # otherwise we simply pass the entire population
      fitness = do.call(fitness.fun, c(list(population$individuals), task$more.args))
    }
    # internally fitness is always a matrix, even in the single-objective case
    if (!is.matrix(fitness)) {
      fitness = matrix(fitness, nrow = 1L)
    }
    return(fitness)
  }



  # otherwise do or do not parallelization via parallelMap
  # We need this wrapper to distinguish between functions of signature
  # fun(x, ...) where x is a list of parameters of the parameter set and
  # fun(x, y, z, ...) where each parameter corresponds to an argument.
  wrapFun = if (getParamNr(task$par.set) == 1L) list else identity
  fitness = parallelMap(function(x) do.call(fitness.fun, c(wrapFun(x), task$more.args)),
    population$individuals, level = "ecr.evaluateFitness")
  # force fitness to be stored in a matrix (be consistent for single and
  # multi-objective fitness funs)
  fitness = do.call(cbind, fitness)
  return(fitness)
}
