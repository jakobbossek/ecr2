#' @title Default monitor.
#'
#' @description Default monitor object that outputs messages to the console
#' based on a default logger (see \code{initLogger}).
#'
#' @param step [\code{integer(1)}]\cr
#'   Number of steps of the EA between monitoring.
#'   Default is 10.
#' @return [\code{ecr_monitor}]
#' @export
setupECRDefaultMonitor = function(step = 10L) {
  step = asInt(step)
  force(step)

  monitor = makeECRMonitor(
    before = function(log, ...) {
      catf("Starting optimization process.")
    },
    step = function(log, ...) {
      assertClass(log, "ecr_logger")
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
