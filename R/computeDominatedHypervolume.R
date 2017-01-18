#' @title
#' Functions for the calculation of the dominated hypervolume (contribution).
#'
#' @description
#' The function \code{computeDominatedHypervolume} computes the dominated
#' hypervolume of a set of points given a reference set whereby
#' \code{computeHypervolumeContribution} computes the hypervolume contribution
#' of each point.
#'
#' If no reference point is given the nadir point of the set \code{x} is
#' determined and a positive offset with default 1 is added. This is to ensure
#' that the reference point dominates all of the points in the reference set.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points (column-wise).
#' @param ref.point [\code{numeric} | \code{NULL}]\cr
#'   Reference point. Set to the maximum in each dimension by default if not provided.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset to be added to each component of the reference point only in the case
#'   where no reference is provided and one is calculated automatically.
#' @return [\code{numeric(1)}] Dominated hypervolume in the case of
#'  \code{computeDominatedHypervolume} and the dominated hypervolume of a single
#'  point in the case of \code{computeHypervolumeContribution}.
#' @rdname dominated_hypervolume
#' @export
computeDominatedHypervolume = function(x, ref.point = NULL) {
  # sanity checks
  assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)

  if (is.null(ref.point)) {
    ref.point = apply(x, 1L, max)
  }

  if (any(is.infinite(x))) {
    warningf("Set of points contains %i infinite values.", sum(is.infinite(x)))
    return(NaN)
  }

  if (length(ref.point) != nrow(x)) {
    stopf("Set of points and reference point need to have the same dimension, but
      set of points has dimension %i and reference point has dimension %i.", nrow(x), length(ref.point))
  }

  if (any(is.infinite(ref.point))) {
    warningf("Reference point contains %i infinite values.", sum(is.infinite(ref.point)))
    return(NaN)
  }

  return(.Call("computeDominatedHypervolumeC", x, ref.point))
}

#' @export
#' @rdname dominated_hypervolume
computeHypervolumeContribution = function(x, ref.point = NULL, offset = 1) {
  if (is.null(ref.point)) {
    ref.point = approximateNadirPoint(x) + offset
  }
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  assertNumeric(ref.point, any.missing = FALSE)
  assertNumber(offset, finite = TRUE, lower = 0)

  return(.Call("computeDominatedHypervolumeContributionC", x, ref.point))
}
