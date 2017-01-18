#' @title
#' Stopping condition: close to optimum.
#'
#' @description
#' Stop the EA if the best fitness value is close to the given optimum.
#'
#' @param eps [\code{numeric(1)}]\cr
#'   Maximal deviation from optimal value.
#' @param opt [\code{numeric(1)}]\cr
#'   Optimal value.
#' @return [\code{function}]
#' @family stopping conditions
#' @export
setupCloseToOptimumTerminator = function(eps = 1e-4, opt = NULL) {
  assertNumber(eps, lower = 0, na.ok = FALSE)
  assertNumber(opt, na.ok = FALSE)

  force(eps)
  force(opt)

  condition.fun = function(opt.state) {
    task = getOptStateTask(opt.state)
    if (task$n.objectives > 1L) {
      stopf("CloseToOptimum stopping condition is suited only for the single-objective case.")
    }
    best.fitness = getOptStateBestIndividual(opt.state)$value
    return(abs(best.fitness - opt) < eps)
  }

  makeTerminator(
    condition.fun,
    name = "CloseToOptimum",
    message = sprintf("At most %f deviation from optimum %f:", eps, opt)
  )
}
