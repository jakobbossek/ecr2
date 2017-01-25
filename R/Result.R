#' @title
#' Result object.
#'
#' @description
#' S3 object returned by \code{\link{doTheEvolution}} containing the best found
#' parameter setting and value in the single-objective case and the Pareto-front/-set
#' in case of a multi-objective optimization problem. Moreover a set of further
#' information, e.g., reason of termination, the control object etc. are returned.
#'
#' The single objective result object contains the following fields:
#' \describe{
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{control}{The \code{ecr_control} object passed to \code{\link{doTheEvolution}}.}
#'   \item{best.param}{Overall best parameter setting.}
#'   \item{best.value}{Overall best objective value.}
#'   \item{last.population}{Last population.}
#'   \item{message}{Character string describing the reason of termination.}
#' }
#'
#' In case of a solved multi-objective function the result object contains the
#' following fields:
#' \describe{
#'   \item{task}{The \code{ecr_optimization_task}.}
#'   \item{control}{The \code{ecr_control} object passed to \code{\link{doTheEvolution}}.}
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
setupResult.ecr_single_objective = function(population, fitness, control, stop.object) {
  logger = control$logger
  makeS3Obj(
    task = control$task,
    best.x = logger$env$best.x,
    best.y = logger$env$best.y,
    log = logger,
    last.population = population,
    last.fitness = as.numeric(fitness),
    message = stop.object$message,
    classes = c("ecr2_single_objective_result", "ecr2_result")
  )
}

#' @export
print.ecr_single_objective_result = function(x, ...) {
  minmax = ifelse(x$task$minimize, "minimization", "maximization")
  catf("EA applied to solve single-objective %s problem.", minmax)
  catf("Best found value: %.6f", x$best.value)
  if (isSmoofFunction(x$task$fitness.fun)) {
    n = length(x$best.param)
    l = min(n, 1L)
    pars = collapse(x$best.param[seq(l)], sep = ", ")
    if (l < n)
      pars = paste(pars, ", ...")
    catf("Best found parameters: %s", pars)
  }
  printAdditionalInformation(x)
}

#' @export
setupResult.ecr_multi_objective = function(population, fitness, control, stop.object) {
  pareto.idx = which.nondominated(fitness)
  pareto.front = as.data.frame(t(fitness[, pareto.idx, drop = FALSE]))
  colnames(pareto.front) = control$task$objective.names
  makeS3Obj(
    task = control$task,
    log = control$logger,
    pareto.idx = pareto.idx,
    pareto.front = pareto.front,
    pareto.set = population[pareto.idx],
    last.population = population,
    message = stop.object$message,
    classes = c("ecr2_multi_objective_result", "ecr2_result")
  )
}

#' @export
print.ecr_multi_objective_result = function(x, ...) {
  obj = ifelse(x$task$n.objectives == 2L, "bi-objective", "many-objective")
  catf("EA applied to solve %s problem.", obj)
  catf("Number of nondominanted points: %i", nrow(x$pareto.front))
  print(BBmisc::printHead(x$pareto.front))
}

printAdditionalInformation = function(x) {
  catf(x$message)
  catf("Generations: %i", x$final.opt.state$iter)
  catf("Evaluations: %i", x$final.opt.state$n.evals)
}

