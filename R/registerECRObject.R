#' Register object to control object.
#'
#' @template arg_control
#' @param name [\code{character(1)}]\cr
#'   Name to use when object is stored to control object.
#' @param object [any]\cr
#'   Object to store.
#' @param fun [\code{function}]\cr
#'   Function to store.
#' @param
#' @return [\code{ecr2_control}]
#'   Modified control object.
#' @export
registerECRObject = function(control, name, fun) {
  assertClass(control, "ecr2_control")
  assertString(name)
  #FIXME: check if name already exists
  control[[name]] = fun
  return(control)
}

registerGenerator = function(control, fun) {
  assertClass(fun, "ecr2_generator")
  registerECRObject(control, "generate", fun)
}

registerMutator = function(control, fun) {
  assertClass(fun, "ecr2_mutator")
  registerECRObject(control, "mutate", fun)
}

registerRecombinator = function(control, fun) {
  assertClass(fun, "ecr2_recombinator")
  registerECRObject(control, "recombine", fun)
}

registerSurvivalSelector = function(control, fun) {
  assertClass(fun, "ecr2_selector")
  registerECRObject(control, "selectForSurvival", fun)
}

registerObjectiveFunction = function(control, fun, n.objectives = NULL, minimize = NULL, objective.names = NULL) {
  task = makeOptimizationTask(fun, n.objectives, minimize, objective.names)
  registerECRObject(control, "task", task)
}

registerLogger = function(control, logger) {
  assertClass(logger, "ecr2_monitor")
  registerECRObject(control, "logger", logger)
}
