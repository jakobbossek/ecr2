#' @title
#' Stopping condition: maximum number of function evaluations.
#'
#' @description
#' Stop the EA after at most \code{max.evals} evaluations of the fitness function.
#'
#' @param max.evals [\code{integer(1)}]\cr
#'   Maximal number of function evaluations. Default ist \code{Inf}.
#' @return [\code{function}]
#' @family stopping conditions
#' @export
setupMaximumEvaluationsTerminator = function(max.evals = NULL) {
  force(max.evals)

  if (!is.null(max.evals)) {
    assertInt(max.evals, lower = 1L, na.ok = FALSE)
  } else {
    max.evals = Inf
  }

  condition.fun = function(opt.state) {
    return(opt.state$n.evals >= max.evals)
  }

  makeTerminator(
    condition.fun,
    name = "FunctionEvaluationsLimit",
    message = sprintf("Maximum number of objective function evaluations reached: %s",
      if (is.infinite(max.evals)) "Inf" else as.integer(max.evals))
  )
}
