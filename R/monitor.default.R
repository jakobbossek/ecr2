#' @title Default logger.
#'
#' @description The default logger of \pkg{ecr2} is responsable to keep track
#' of the evolutionary process. It is a fundamental building block of the
#' \code{ecr} function where it logs statistics on the population and its fitness
#' values as well as the state of the optimization progress. It is passed to the
#' internal stopping conditions in order to check whether the evolutionary process
#' should terminate.
#'
#' @details
#' Technically the logger is a S3 object, i.e., a
#' list with an additional class attribute \code{ecr2_monitor} and three internal
#' functions \code{before}, \code{step} and \code{after}, which are meant to be
#' called before the evolutionary loop starts, right after each generation and
#' once after the process terminated.
#'
#' @param step [\code{integer(1)}]\cr
#'   Number of steps of the EA between monitoring.
#'   Default is 10.
#' @return [\code{ecr2_monitor}]
#' @export
setupECRDefaultMonitor = function(step = 10L) {
  step = asInt(step)
  force(step)

  monitor = makeECRMonitor(
    before = function(log, ...) {
      catf("Starting optimization process.")
    },
    step = function(log, ...) {
      assertClass(log, "ecr2_logger")
      gen = log$env$n.gens
      if (gen %% step == 0)
        catf("Generation: %i", gen)
    },
    after = function(log, ...) {
      catf("DONE")
    }
  )
  return(monitor)
}
