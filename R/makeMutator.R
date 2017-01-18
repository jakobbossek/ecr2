#' @title
#' Construct a mutation operator.
#'
#' @description
#' Helper function which constructs a mutator, i. e., a mutation operator.
#'
#' @param mutator [\code{function}]\cr
#'   Actual mutation operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the mutator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the mutator works.
#' @param params [\code{list}]\cr
#'   Named list of the parameters the operator has been initialized with.
#'   Default is the empty list.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @return [\code{ecr_mutator}]
#'   Mutator object.
#' @export
makeMutator = function(
  mutator, name, description,
  supported = getAvailableRepresentations(),
  params = list()) {
  assertFunction(mutator, args = c("ind", "task", "control"), ordered = TRUE)
  mutator = makeOperator(mutator, name, description, supported, params)
  mutator = addClasses(mutator, c("ecr_mutator"))
  return(mutator)
}

# Helper function which returns all supported parameter representations.
getAvailableRepresentations = function() {
  c("permutation", "binary", "float", "custom")
}
