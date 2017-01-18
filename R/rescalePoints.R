#' @title
#' Rescaling of points.
#'
#' @description
#' Rescales a cloud of points so that all the point are located within the
#' bounds given by \code{lower} and \code{upper}. If both parameters are missing
#' the minimal/maximal values per dimension are extracted.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points.
#' @param lower [\code{numeric(1)}]\cr
#'   Lower bound of the new bounding box.
#' @param upper [\code{numeric(1)}]\cr
#'   Upper bound of the new bounding box.
#' @return [\code{matrix}]
#' @export
rescalePoints = function(x, lower = NULL, upper = NULL) {
  assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  n = ncol(x)
  if (is.null(lower)) {
    lower = apply(x, 1L, min)
  }
  if (is.null(upper)) {
    upper = apply(x, 1L, max)
  }
  assertNumeric(lower, len = n, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(upper, len = n, any.missing = FALSE, all.missing = FALSE)

  return((x - lower) / (upper - lower))
}
