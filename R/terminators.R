#' @title
#' Stopping condition: maximum number of function evaluations.
#'
#' @description
#' Stop the EA after at most \code{max.evals} evaluations of the fitness function.
#'
#' @param max.evals [\code{integer(1)}]\cr
#'   Maximal number of function evaluations. Default ist \code{Inf}.
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @return [\code{ecr2_terminator}]
#' @family stopping conditions
#' @rdname stoppingConditions
#' @name stoppingConditions
stopOnEvals = function(max.evals = NULL) {
  force(max.evals)

  if (!is.null(max.evals)) {
    assertInt(max.evals, lower = 1L, na.ok = FALSE)
  } else {
    max.evals = Inf
  }

  condition.fun = function(log) {
    return(log$env$n.evals >= max.evals)
  }

  makeTerminator(
    condition.fun,
    name = "FunctionEvaluationsLimit",
    message = sprintf("Maximum number of objective function evaluations reached: %s",
      if (is.infinite(max.evals)) "Inf" else as.integer(max.evals))
  )
}

#' @rdname stoppingConditions
#' @export
stopOnIters = function(max.iter = NULL) {
  if (!is.null(max.iter)) {
    assertInt(max.iter, lower = 1L, na.ok = FALSE)
  } else {
    max.iter = Inf
  }
  force(max.iter)

  condition.fun = function(log) {
    return(log$env$n.gens >= max.iter)
  }

  makeTerminator(
    condition.fun,
    name = "IterLimit",
    message = sprintf("Maximum number of iterations reached: %s",
      if (is.infinite(max.iter)) "Inf" else max.iter)
  )
}
