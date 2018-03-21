# @title
# Check whether an operator can handle a specific representation.
#
# @param operator [ecr_operator]
#   Operator.
# @param representation [character(1)]
#   Representation, i.e., float, binary, permutation or custom.
# @return [logical(1)]
checkOperatorIsCompatible = function(operator, representation) {
  if (!is.supported(operator, representation)) {
    stopf("Operator '%s' is not compatible with representation '%s'",
      getOperatorName(operator), representation
    )
  }
}

# @title
# Check operators for compatibility with objectives and representation.
#
# @param control [ecr_control]
#   Control object.
# @return Nothing
checkOperatorCompatibility = function(control) { # nocov start
  task = control$task
  selectors = list(control$selectForMating, control$selectForSurvival)
  desired.obj = if (task$n.objectives == 1L) "single-objective" else "multi-objective"
  lapply(selectors, function(selector) {
    if (desired.obj %nin% attr(selector, "supported.objectives")) {
      stopf("Selector '%s' cannot be applied to problem with %i objectives.",
        getOperatorName(selector), task$n.objectives)
    }
  })

  operators = list(control$mutate, control$recombine, control$generate)
  operators = BBmisc::filterNull(operators)
  lapply(operators, function(operator) {
    if (!is.supported(operator, control$type)) {
      stopf("Operator '%s' is not compatible with representation '%s'.",
        getOperatorName(operator), control$type)
    }
  })
} # nocov end
