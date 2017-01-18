#' @title
#' Stopping condition: maximum number of iterations.
#'
#' @description
#' Stop the EA after at most \code{max.iter} generations/iterations.
#'
#' @param max.iter [\code{integer(1)}]\cr
#'   Maximal number of iterations. Default ist \code{Inf}.
#' @return [\code{function}]
#' @family stopping conditions
#' @export
setupMaximumIterationsTerminator = function(max.iter = NULL) {
  if (!is.null(max.iter)) {
    assertInt(max.iter, lower = 1L, na.ok = FALSE)
  } else {
    max.iter = Inf
  }
  force(max.iter)

  condition.fun = function(opt.state) {
    return(opt.state$iter >= max.iter)
  }

  makeTerminator(
    condition.fun,
    name = "IterLimit",
    message = sprintf("Maximum number of iterations reached: %s",
      if (is.infinite(max.iter)) "Inf" else max.iter)
  )
}
