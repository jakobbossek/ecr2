#' @title
#' Stopping condition: time limit.
#'
#' @description
#' Stop the EA after a maximal time budget.
#'
#' @param max.time [\code{integer(1)}]\cr
#'   Time budget in seconds. Default ist \code{Inf}.
#' @return [\code{function}]
#' @family stopping conditions
#' @export
setupMaximumTimeTerminator = function(max.time = NULL) {
  if (!is.null(max.time)) {
    assertInt(max.time, lower = 1L, na.ok = FALSE)
  } else {
    max.time = Inf
  }
  force(max.time)

  condition.fun = function(opt.state) {
    return(opt.state$time.passed >= max.time)
  }

  makeTerminator(
    condition.fun,
    name = "TimeLimit",
    message = sprintf("Time limit reached: '%s' [seconds]", max.time)
  )
}
