#' @title
#' Stopping conditions
#'
#' @description
#' Stop the EA after a fixed number of fitness function evaluations, after
#' a predefined number of generations/itereations or if the known optimal function
#' value is approximated (only for single-objective optimization).
#'
#' @param max.evals [\code{integer(1)}]\cr
#'   Maximal number of function evaluations. Default ist \code{Inf}.
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @param opt.y [\code{numeric(1)}]\cr
#'   Optimal scalar fitness function value.
#' @param eps [\code{numeric(1)}]\cr
#'   Stop if absolute deviation from \code{opt.y} is lower than \code{eps}.
#' @return [\code{ecr_terminator}]
#' @family stopping conditions
#' @rdname stoppingConditions
#' @name stoppingConditions
#' @export
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

#' @rdname stoppingConditions
#' @export
stopOnOptY = function(opt.y, eps) {
  assertNumber(eps, lower = 0)
  assertNumber(opt.y)

  force(opt.y)
  force(eps)

  condition.fun = function(log) {
    stats = log$env$stats
    cur.it = log$env$cur.line
    if (!("fitness.min" %in% names(stats))) {
      warningf("Terminator 'stopOnOptY' needs column 'min' in log. Not found!")
      return(FALSE)
    }
    return(abs(stats[cur.it, "fitness.min"] - opt.y) < eps)
  }

  makeTerminator(
    condition.fun,
    name = "OptYApprox",
    message = sprintf("Best function value close to optimum |f_opt - f_EA| < %.5f.",
      eps)
  )
}
