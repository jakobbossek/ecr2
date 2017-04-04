#' @title
#' Factory method for monitor objects.
#'
#' @description
#' Monitor objects serve for monitoring the optimization process, e.g., printing
#' some status messages to the console. Each monitor includes the functions
#' \code{before}, \code{step} and \code{after}, each being a function and expecting
#' a log \code{log} of type \code{ecr_logger} and \code{...} as the only parameters.
#' This way the logger has access to the entire log.
#'
#' @param before [\code{function}]\cr
#'   Function called one time after initialization of the EA.
#' @param step [\code{function}]\cr
#'   Function applied after each iteration of the algorithm.
#' @param after [\code{function}]\cr
#'   Function applied after the EA terminated.
#' @param ... [\code{any}]\cr
#'   Not used.
#' @return [\code{ecr_monitor}]
#'   Monitor object.
#'
# @example examples/ex_makeMonitor.R
#' @export
makeECRMonitor = function(before = NULL, step = NULL, after = NULL, ...) {
  if (!is.null(before)) assertFunction(before, args = c("log", "..."))
  if (!is.null(step)) assertFunction(step, args = c("log", "..."))
  if (!is.null(after)) assertFunction(after, args = c("log", "..."))
  dummy = function(log, ...) {}
  structure(
    list(
      before = coalesce(before, dummy),
      step = coalesce(step, dummy),
      after = coalesce(after, dummy),
      env = new.env()
    ),
    class = "ecr_monitor"
  )
}
