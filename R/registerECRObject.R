#' Register object to control object.
#'
#' @template arg_control
#' @param name [\code{character(1)}]\cr
#'   Name to use when object is stored to control object.
#' @param object [any]\cr
#'   Object to store.
#' @return [\code{ecr2_control}]
#'   Modified control object.
#' @export
registerECRObject = function(control, name, object) {
  assertClass(control, "ecr2_control")
  assertString(name)
  #FIXME: check if name already exists
  control[[name]] = object
  return(control)
}

# Helper function to set operator internally.
#
# @param control [ecr_control]
#   ECR control object.
# @param operator [ecr_operator]
#   The corresponding operator to set.
# @param type [character(1)]
#   The expected type of the operator.
# @param description [character(1)]
#   Short string description of the operator.
# @param field [character(1)]
#   Name of the field in the control object where to store the operator.
# @return [ecr_control]
registerECROperator = function(control, operator, type, description, field) {
  assertClass(control, "ecr2_control")
  if (control$type != "custom" & !is.null(operator)) {
    checkCorrectOperatorType(operator, type, description)
    checkOperatorIsCompatible(operator, control$type)
  }
  control[[field]] = operator
  return(control)
}

registerGenerator = function(control, operator.fun) {
  if (!is.null(operator.fun))
    assertClass(operator.fun, "ecr2_generator")
  registerECROperator(control, operator.fun, "ecr2_generator", "Generator", "generate")
}

registerMutator = function(control, operator.fun) {
  if (!is.null(operator.fun))
    assertClass(operator.fun, "ecr2_mutator")
  registerECROperator(control, operator.fun, "ecr2_mutator", "Mutator", "mutate")
}

registerRecombinator = function(control, operator.fun) {
  if (!is.null(operator.fun))
    assertClass(operator.fun, "ecr2_recombinator")
  registerECROperator(control, operator.fun, "ecr2_recombinator", "Recombinator", "recombine")
}

registerSurvivalSelector = function(control, operator.fun) {
  assertClass(operator.fun, "ecr2_selector")
  registerECROperator(control, operator.fun, "ecr2_selector", "Survival selector", "selectForSurvival")
}

registerMatingSelector = function(control, operator.fun) {
  assertClass(operator.fun, "ecr2_selector")
  registerECROperator(control, operator.fun, "ecr2_selector", "Parent selector", "selectForMating")
}

registerObjectiveFunction = function(control, fun, n.objectives = NULL, minimize = NULL, objective.names = NULL) {
  task = makeOptimizationTask(fun, n.objectives, minimize, objective.names)
  registerECRObject(control, "task", task)
}

registerLogger = function(control, logger) {
  assertClass(logger, "ecr2_monitor")
  registerECRObject(control, "logger", logger)
}
