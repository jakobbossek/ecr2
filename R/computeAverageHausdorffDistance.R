#' @title
#' Average Hausdorff Distance computation.
#'
#' @description
#' Computes the average Hausdroff distance measure between two point sets.
#'
#' @template arg_pointset_A
#' @template arg_pointset_B
#' @param p [\code{numeric(1)}]\cr
#'   Parameter p of the average Hausdoff metric.
#'   Default is 1.
#' @param normalize [\code{logical(1)}]\cr
#'   Should the front be normalized on basis of \code{B}?
#'   Default is \code{FALSE}.
#' @template arg_asemoa_dist_fun
#' @return [\code{numeric(1)}] Average Hausdorff distance of sets \code{A} and \code{B}.
#' @export
computeAverageHausdorffDistance = function(A, B, p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance) {
  # sanity check imput
  assertMatrix(A, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertMatrix(B, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  if (nrow(A) != nrow(B)) {
    stopf("Sets A and B need to have the same dimensionality.")
  }
  assertNumber(p, lower = 0.0001, na.ok = FALSE)
  assertFlag(normalize)

  # ac
  GD = computeGenerationalDistance(A, B, p, normalize, dist.fun)
  IGD = computeInvertedGenerationalDistance(A, B, p, normalize, dist.fun)
  delta = max(GD, IGD)
  return(delta)
}

computeEuclideanDistance = function(x) {
  sqrt(sum(x^2))
}

#' @title
#' Computes Generational Distance.
#'
#' @description
#' Helper to compute the Generational Distance (GD) between two sets of points.
#'
#' @inheritParams computeAverageHausdorffDistance
#' @return [\code{numeric(1)}]
#' @export
computeGenerationalDistance = function(A, B, p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance) {
  assertMatrix(A, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertMatrix(B, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertFlag(normalize)
  if (nrow(A) != nrow(B)) {
    stopf("Sets A and B need to have the same dimensionality.")
  }
  assertNumber(p, lower = 0.0001, na.ok = FALSE)

  if (normalize) {
    min.B = apply(B, 1L, min)
    max.B = apply(B, 1L, max)
    A = normalizeMatrix(A, min.value = min.B, max.value = max.B)
    B = normalizeMatrix(B, min.value = min.B, max.value = max.B)
  }

  # compute distance of each point from A to the point set B
  # dists = apply(A, 2L, function(a) computeDistanceFromPointToSetOfPoints(a, B, dist.fun))
  # GD = mean(dists^p)^(1 / p)
  GD = .Call(`_ecr_computeGenerationalDistanceC`, A, B, p)
  return(GD)
}

#' @title
#' Computes Inverted Generational Distance.
#'
#' @description
#' Helper to compute the Inverted Generational Distance (IGD) between two sets
#' of points.
#'
#' @inheritParams computeAverageHausdorffDistance
#' @return [\code{numeric(1)}]
#' @export
computeInvertedGenerationalDistance = function(A, B, p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance) {
  return(computeGenerationalDistance(B, A, p, normalize, dist.fun))
}

#' @title
#' Computes distance between a single point and set of points.
#'
#' @description
#' Helper to compute distance between a single point and a point set.
#'
#' @param a [\code{numeric(1)}]\cr
#'   Point given as a numeric vector.
#' @param B [\code{matrix}]\cr
#'   Point set (each column corresponds to a point).
#' @template arg_asemoa_dist_fun
#' @return [\code{numeric(1)}]
#' @export
computeDistanceFromPointToSetOfPoints = function(a, B, dist.fun = computeEuclideanDistance) {
  dists = computeDistancesFromPointToSetOfPoints(a, B, dist.fun)
  return(min(dists))
}

computeDistancesFromPointToSetOfPoints = function(a, B, dist.fun = computeEuclideanDistance) {
  # to avoid loops here we construct a matrix and make use of R's vector
  # computation qualities
  tmp = matrix(rep(a, each = ncol(B)), nrow = nrow(B), byrow = TRUE)
  dists = apply(tmp - B, 2L, function(x) {
    dist.fun(x)
  })
  return(dists)
}
