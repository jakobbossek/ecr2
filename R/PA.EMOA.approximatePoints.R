#' @title
#' Reference point approximations.
#'
#' @description
#' Helper functions to compute nadir or ideal point from sets of
#' points, e.g., multiple approximation sets.
#'
#' @param ... [\code{matrix}]\cr
#'   Arbirary number of matrizes.
#' @param sets [\code{list}]\cr
#'   List of matrizes. This is an alternative way of passing the sets. Can be used
#'   exclusively or combined with \code{...}.
#' @return [\code{numeric}] Reference point.
#' @rdname reference_point_approximation
#' @family EMOA performance assessment tools
#' @export
approximateNadirPoint = function(..., sets = NULL) {
  return(approximatePoint(..., sets = sets, FUN = max))
}

#' @export
#' @rdname reference_point_approximation
approximateIdealPoint = function(..., sets = NULL) {
  return(approximatePoint(..., sets = sets, FUN = min))
}

# @title
# Helper to compute the nadir/ideal point.
#
# @description The functions expects a set of sets and a function FUN to apply.
#
# @param ... [any]\cr
#   Arbitrary number of matrizes.
# @param sets [\code{list}]\cr
#   List of sets. This is an alternative way of passing the sets. Can be used
#   exclusively or combined with \code{...}.
# @param FUN [\code{function}]\cr
#   Either min or max function.
# @return [\code{numeric}]
approximatePoint = function(..., sets = NULL, FUN) {
  assertFunction(FUN)

  # we can combine both types of parameter passing here
  sets2 = list(...)
  if (!is.null(sets)) {
    assertList(sets, types = "matrix")
  }
  sets = c(sets, sets2)
  assertListOfPointSets(sets)

  # sapply returns a matrix with each row corresponding to the nadir point
  # of a single set. We hence apply the FUN rowwise again.
  point = apply(sapply(sets, function(set) {
    apply(set, 1L, FUN)
  }), 1L, FUN)

  return(point)
}

# @title
# Checks for point set.
#
# @description
# Helper function to check if all given sets have the same dimension, i.e.,
# number of objectives.
#
# @param x [list]
#   List of sets, i.e., matrizes.
assertListOfPointSets = function(x) {
  assertList(x, types = "matrix")
  n.rows = sapply(x, nrow)
  if (length(unique(n.rows)) > 1L) {
    stopf("All sets need to be of the same dimension.")
  }
}
