#' @title
#' Construct a mutation operator.
#'
#' @description
#' Helper function which constructs a mutator, i. e., a mutation operator.
#'
#' @param mutator [\code{function}]\cr
#'   Actual mutation operator.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @return [\code{ecr_mutator}]
#'   Mutator object.
#' @export
makeMutator = function(
  mutator,
  supported = getAvailableRepresentations()) {
  assertFunction(mutator, args = c("ind", "par.list"), ordered = TRUE)
  mutator = makeOperator(mutator, supported)
  mutator = addClasses(mutator, c("ecr2_mutator"))
  return(mutator)
}

# Helper function which returns all supported parameter representations.
getAvailableRepresentations = function() {
  c("permutation", "binary", "float", "custom")
}
