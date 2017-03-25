#' Helper function to set operator internally.
#'
#' @param control [\code{ecr_control}]\cr
#'   ECR control object.
#' @param slot [\code{character(1)}]\cr
#'   Name of the field in the control object where to store the operator.
#' @param fun [\code{function}]\cr
#'   Actual operator. In order to use the various helper functions of ecr one needs
#'   to stick to a simple convention: The first argument of \code{function} should
#'   be the individual to mutate, a list of individuals for recombination or a matrix
#'   of fitness values for recombination. If one does not want to use the corresponding
#'   helpers, e.g., \code{mutate}, the signature of the function does not matter. However,
#'   in this case you are responsable to pass arguments correctly.
#' @param ... [any]\cr
#'   Further arguments for \code{fun}. These arguments are stored in the control object
#'   and passed on to \code{fun}.
#' @return [\code{ecr_control}]
registerECROperator = function(control, slot, fun, ...) {
  assertClass(control, "ecr2_control")
  assertString(slot)
  assertFunction(fun)
  fun.pars = list(...)
  if (!is.null(control[[slot]]))
    stopf("Operator '%s' already present.", slot)
  control[[paste0(slot, ".pars")]] = fun.pars
  control[[slot]] = fun
  return(control)
}

#' @title Register control parameters
#'
#' @description Add further parameters which are neccessary for an operator or
#' several operators. These are passed down automatically to all operator calls
#' when using, e.g., \code{generateOffspring}.
#'
#' @template arg_control
#' @param ... [any]\cr
#'    Further control parameters appended to the control object.
#' @return [\code{ecr2_control}]
#' @export
registerECRParams = function(control, ...) {
  passed.params = list(...)
  control$params = BBmisc::insert(control$params, passed.params)
  return(control)
}

# registerObjectiveFunction = function(control, fun, n.objectives = NULL, minimize = NULL, objective.names = NULL) {
#   task = makeOptimizationTask(fun, n.objectives, minimize, objective.names)
#   registerECRObject(control, "task", task)
# }

# registerLogger = function(control, logger) {
#   assertClass(logger, "ecr2_monitor")
#   registerECRObject(control, "logger", logger)
# }
