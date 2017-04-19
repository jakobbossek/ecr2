#' @title
#' Control object generator.
#'
#' @description
#' The control object keeps information on the objective function and a set of
#' evolutionary components, i.e., operators.
#'
#' @template arg_fitness_fun
#' @template arg_n_objectives
#' @template arg_minimize
#' @return [\code{ecr_control}]
#' @name initECRControl
#' @rdname initECRControl
#' @export
initECRControl = function(fitness.fun, n.objectives = NULL, minimize = NULL) {
  task = makeOptimizationTask(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  makeS3Obj("ecr_control", task = task)
}

#FIXME: converter soobench -> params
#FIXME: converter moobench -> params

extractFunctionParameters = function(fun) {
  UseMethod("extractFunctionParameters")
}

extractFunctionParameters.smoof_function = function(fun) {
  par.set = getParamSet(fun)
  return(list(
    n.objectives = getNumberOfObjectives(fun),
    n.dim = getNumberOfParameters(fun),
    minimize = shouldBeMinimized(fun),
    upper = getUpper(par.set),
    lower = getLower(par.set))
  )
}

extractFunctionParameters.function = function(fun) {
  return(list())
}

extractFunctionParameters.smoof_wrapped_function = function(fun) {
  extractFunctionParameters(getWrappedFunction(fun))
}

initControlParams = function(control, ...) {
  control$params = list(...)
  return(control)
}

initDefaultOperators = function(control, type, n.objectives) {
  n.objectives = asInt(n.objectives, lower = 1L)
  assertChoice(type, c("float", "permutation", "binary", "custom"))
  obj.type = if (n.objectives == 1L) "single" else "multi"
  control = registerECROperator(control, "selectForMating", getDefaultEvolutionaryOperators(type, "parent.selector", n.objectives, control))
  control = registerECROperator(control, "selectForSurvival", getDefaultEvolutionaryOperators(type, "survival.selector", n.objectives, control))
  control = registerECROperator(control, "mutate", getDefaultEvolutionaryOperators(type, "mutator", n.objectives, control))
  control = registerECROperator(control, "recombine", getDefaultEvolutionaryOperators(type, "recombinator", n.objectives, control))
  return(control)
}

# @title
# Check if given operator is of the specified type.
#
# @param operator [ecr_operator]
#   Operator.
# @param class [character(1)]
#   Class.
# @param type [character(1)]
#   Type of the operator.
# @return Nothing
checkCorrectOperatorType = function(operator, class, type) {
  if (!inherits(operator, class)) {
    stopf("%s must be of class '%s', not '%s'.", type, class, collapse(attr(operator, "class"), sep = ", "))
  }
}

# @title
# Helper function which returns the defaults evolutionary operators for the
# standard representations.
#
# @param representation [\code{character(1)}]\cr
#   Genotype representation of the parameters. Available are binary, real,
#   permutation and custom.
# @param type [\code{character(1)}]\cr
#   Type of evolutionary operator. Possible are parent.selector, generator,
#   mutator, recombinator and survival.selector.
# @return [\code{ecr_operator}]
getDefaultEvolutionaryOperators = function(representation, type, n.objectives, control) {
  if (n.objectives == 1L) {
    return(getSingleObjectiveDefaults(representation, type, control))
  }
  return(getMultiObjectiveDefaults(representation, type, control))
}

getSingleObjectiveDefaults = function(representation, type, control) {
  defaults = list(
    "float" = list(
      "parent.selector" = setup(selTournament, k = 2L),
      "mutator" = try(setup(mutGauss), silent = TRUE),
      "recombinator" = setup(recIntermediate),
      "survival.selector" = setup(selGreedy)
    ),
    "binary" = list(
      "parent.selector" = setup(selTournament, k = 2L),
      "mutator" = setup(mutBitflip),
      "recombinator" = setup(recCrossover),
      "survival.selector" = setup(selGreedy)
    ),
    "permutation" = list(
      "parent.selector" = setup(selTournament, k = 2L),
      "mutator" = setup(mutSwap),
      "recombinator" = setup(recPMX),
      "survival.selector" = setup(selGreedy)
    ),
    "custom" = list(
      "parent.selector" = setup(selTournament, k = 2L),
      "mutator" = NULL,
      "recombinator" = NULL,
      "survival.selector" = setup(selGreedy)
    )
  )

  if (representation %in% names(defaults)) {
    return(defaults[[representation]][[type]])
  }
  stopf("No defaults availiable for custom representation. You need to specify all
    operators by hand.")
}

getMultiObjectiveDefaults = function(representation, type, control) {
  defaults = list(
    "float" = list(
      "parent.selector" = setup(selSimple),
      "mutator" = try(setup(mutGauss), silent = TRUE),
      "recombinator" = setup(recIntermediate),
      "survival.selector" = setup(selNondom)
    ),
    "binary" = list(
      "parent.selector" = setup(selSimple),
      "mutator" = setup(mutBitflip),
      "recombinator" = setup(recCrossover),
      "survival.selector" = setup(selNondom)
    ),
    "permutation" = list(
      "parent.selector" = setup(selSimple),
      "mutator" = setup(mutSwap),
      "recombinator" = setup(recPMX),
      "survival.selector" = setup(selNondom)
    ),
    "custom" = list(
      "parent.selector" = setup(selSimple),
      "mutator" = NULL,
      "recombinator" = NULL,
      "survival.selector" = setup(selNondom)
    )
  )

  if (representation %in% names(defaults)) {
    return(defaults[[representation]][[type]])
  }
  stopf("No defaults availiable for custom representation. You need to specify all
    operators by hand.")
}
