#' @title
#' Construct a recombination operator.
#'
#' @description
#' Helper function which constructs a recombinator, i. e., a recombination operator.
#'
#' @note If a recombinator returns more than one child, the \code{multiple.children}
#' parameter needs to be \code{TRUE}, which is the default. In case of multiple
#' children produced these have to be placed within a list.
#'
#' @param recombinator [\code{function}]\cr
#'   Actual mutation operator.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. Possible choices:
#'   \dQuote{permutation}, \dQuote{float}, \dQuote{binary} or \dQuote{custom}.
#' @param n.parents [\code{integer(1)}]\cr
#'   Number of parents supported.
#' @param n.children [\code{integer(1)}]\cr
#'   How many children does the recombinator produce?
#'   Default is \code{1}.
#' @return [\code{ecr_recombinator}]
#'   Recombinator object.
#' @export
makeRecombinator = function(
  recombinator,
  supported = getAvailableRepresentations(),
  n.parents = 2L,
  n.children = NULL) {
  assertFunction(recombinator)
  assertInt(n.parents, lower = 2L)
  assertInt(n.children, lower = 1L)

  recombinator = makeOperator(recombinator, supported)
  attr(recombinator, "n.parents") = n.parents
  attr(recombinator, "n.children") = n.children

  recombinator = addClasses(recombinator, c("ecr_recombinator"))

  return(recombinator)
}

generatesMultipleChildren = function(recombinator) {
  UseMethod("generatesMultipleChildren")
}

generatesMultipleChildren.ecr_recombinator = function(recombinator) {
  return(attr(recombinator, "n.children") > 0L)
}

getNumberOfParentsNeededForMating = function(recombinator) {
  UseMethod("getNumberOfParentsNeededForMating")
}

getNumberOfParentsNeededForMating.ecr_recombinator = function(recombinator) {
  return(attr(recombinator, "n.parents"))
}

getNumberOfChildren = function(recombinator) {
  UseMethod("getNumberOfChildren")
}

getNumberOfChildren.ecr_recombinator = function(recombinator) {
  return(attr(recombinator, "n.children"))
}
