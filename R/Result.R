#' @title
#' Result object.
#'
#' @description S3 object returned by \code{\link{ecr}} containing the best found
#' parameter setting and value in the single-objective case and the Pareto-front/-set
#' in case of a multi-objective optimization problem. Moreover a set of further
#' information, e.g., reason of termination, the control object etc. are returned.
#'
#' The single objective result object contains the following fields:
#' \describe{
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{best.x}{Overall best parameter setting.}
#'   \item{best.y}{Overall best objective value.}
#'   \item{log}{Logger object.}
#'   \item{last.population}{Last population.}
#'   \item{last.fitness}{Numeric vector of fitness values of the last population.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#'
#' In case of a solved multi-objective function the result object contains the
#' following fields:
#' \describe{
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{log}{Logger object.}
#'   \item{pareto.idx}{Indizes of the non-dominated solutions in the last population.}
#'   \item{pareto.front}{(n x d) matrix of the approximated non-dominated front where n
#'   is the number of non-dominated points and d is the number of objectives.}
#'   \item{pareto.set}{Matrix of decision space values resulting with objective values
#'   given in pareto.front.}
#'   \item{last.population}{Last population.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#'
#' @name ecr_result
#' @rdname ecr_result
NULL

# @title Generator for result object.
#
# @param opt.state [\code{ecr_opt_state}]\cr
#   Optimization state.
# @param stop.object [\code{list}]\cr
#   List of triggered stopping conditions.
# @param control [\code{ecr_control}]\cr
#   Control object.
# @return [\code{ecr_single_objective_result} | \code{ecr_multi_objective_result}]
setupResult = function(population, fitness, control) {
  UseMethod("setupResult")
}

#' @export
setupResult.ecr_single_objective = function(population, fitness, control, log, stop.object) {
  makeS3Obj(
    task = control$task,
    best.x = log$env$best.x,
    best.y = log$env$best.y,
    log = log,
    last.population = population,
    last.fitness = as.numeric(fitness),
    message = stop.object$message,
    classes = c("ecr_single_objective_result", "ecr_result")
  )
}

#' @export
print.ecr_single_objective_result = function(x, ...) {
  minmax = ifelse(x$task$minimize, "minimization", "maximization")
  catf("EA applied to solve single-objective %s problem.", minmax)
  catf("Best found value: %.6f", x$best.y)
  if (isSmoofFunction(x$task$fitness.fun)) {
    n = length(x$best.x)
    l = min(n, 1L)
    pars = collapse(x$best.x[seq(l)], sep = ", ")
    if (l < n)
      pars = paste(pars, ", ...")
    catf("Best found parameters: %s", pars)
  }
}

#' @export
setupResult.ecr_multi_objective = function(population, fitness, control, log, stop.object) {
  pareto.idx = which.nondominated(fitness)
  pareto.front = as.data.frame(t(fitness[, pareto.idx, drop = FALSE]))
  colnames(pareto.front) = control$task$objective.names
  makeS3Obj(
    task = control$task,
    log = log,
    pareto.idx = pareto.idx,
    pareto.front = pareto.front,
    pareto.set = population[pareto.idx],
    last.population = population,
    message = stop.object$message,
    classes = c("ecr_multi_objective_result", "ecr_result")
  )
}

#' @export
print.ecr_multi_objective_result = function(x, ...) {
  obj = ifelse(x$task$n.objectives == 2L, "bi-objective", "many-objective")
  catf("EA applied to solve %s problem.", obj)
  catf("Number of nondominanted points: %i", nrow(x$pareto.front))
  print(BBmisc::printHead(x$pareto.front))
}

