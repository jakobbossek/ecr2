#' @title
#' Computation of the unary epsilon-indicator.
#'
#' @description
#' Functions for the computation of unary and binary measures which
#' are useful for the evaluation of the performace of EMOAs. See the references
#' section for literature on these indicators.
#'
#' Given a set of points \code{points}, \code{computeEpsilonIndicator} computes the
#' unary epsilon-indicator provided a set of reference points \code{ref.points}.
#'
#' The \code{computeHypervolumeIndicator} function computes the hypervolume indicator
#' Hyp(X, R, r). Given a set of point X (\code{points}), another set of reference
#' points R (\code{ref.points}) (which maybe the true Pareto front) and a reference
#' point r (\code{ref.point}) it is defined as Hyp(X, R, r) = HV(X, r) - HV(R, r).
#'
#' @param points [\code{matrix}]\cr
#'   Matrix of points.
#' @param ref.points [\code{matrix}]\cr
#'   Set of reference points.
#' @param ref.point [\code{numeric}]\cr
#'   A single reference point used e.g. for the computation of the hypervolume
#'   indicator via \code{computeHypervolumeIndicator}. If \code{NULL} the
#'   nadir point of the union of the \code{points} and \code{ref.points} is used.
#' @param ideal.point [\code{numeric}]\cr
#'   The utopia point of the true Pareto front, i.e., each component of the point
#'   contains the best value if the other objectives are neglected.
#' @param nadir.point [\code{numeric}]\cr
#'   Nadir point of the true Pareto front.
#' @param lambda [\code{integer(1)}]\cr
#'   Number of weight vectors to use in estimating the utility function.
#' @param utility [\code{character(1)}]\cr
#'   Name of the utility function to use. Must be one of \dQuote{weightedsum},
#'   \dQuote{tschebycheff} or \dQuote{augmented tschbycheff}.
#' @return [\code{numeric(1)}] Epsilon indicator.
#' @rdname emoa_indicators
#' @export
computeEpsilonIndicator = function(points, ref.points) {
  # sanity checks
  assertMatrix(points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertMatrix(ref.points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertSameDimensions(points, ref.points)

  return(.Call("computeEpsilonIndicatorC", points, ref.points))
}

#' @rdname emoa_indicators
#' @export
computeHypervolumeIndicator = function(points, ref.points, ref.point = NULL) {
  # compute nadir point
  if (is.null(ref.point)) {
    ref.point = approximateNadirPoint(points, ref.points)
  }

  # sanity checks
  assertMatrix(points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertMatrix(ref.points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertNumeric(ref.point, any.missing = FALSE, all.missing = FALSE)
  assertSameDimensions(points, ref.points, ref.point)

  # actual indicator calculation
  hv.points = computeDominatedHypervolume(points, ref.point)
  hv.ref.points = computeDominatedHypervolume(ref.points, ref.point)

  return (hv.ref.points - hv.points)
}

#' @rdname emoa_indicators
#' @export
computeR1Indicator = function(points, ref.points, ideal.point = NULL,
  nadir.point = NULL, lambda = NULL, utility = "tschebycheff") {
  computeRIndicator(points, ref.points, ideal.point, nadir.point, lambda, utility,
    aggregator = function(ua, ur) mean(ua > ur) + mean(ua == ur) / 2)
}

#' @rdname emoa_indicators
#' @export
computeR2Indicator = function(points, ref.points, ideal.point = NULL,
  nadir.point = NULL, lambda = NULL, utility = "tschebycheff") {
  computeRIndicator(points, ref.points, ideal.point, nadir.point, lambda, utility,
    aggregator = function(ua, ur) mean(ur - ua))
}

#' @rdname emoa_indicators
#' @export
computeR3Indicator = function(points, ref.points, ideal.point = NULL,
  nadir.point = NULL, lambda = NULL, utility = "tschebycheff") {
  computeRIndicator(points, ref.points, ideal.point, nadir.point, lambda, utility,
    aggregator = function(ua, ur) mean((ur - ua) / ur))
}

# @rdname emoa_indicators
computeRIndicator = function(
  points, ref.points,
  ideal.point = NULL, nadir.point = NULL,
  lambda = NULL,
  utility,
  aggregator) {
  assertMatrix(points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  assertMatrix(ref.points, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  if (is.null(ideal.point)) {
    ideal.point = approximateIdealPoint(points, ref.points)
  }
  assertNumeric(ideal.point, any.missing = FALSE, all.missing = FALSE)
  utilities = c("weightedsum", "tschebycheff", "augmented tschbycheff")
  assertChoice(utility, utilities)
  assertFunction(aggregator)

  # convert utility to integer index which is used by the C code
  utility = which((match.arg(utility, utilities)) == utilities)
  utility = as.integer(utility)

  n.obj = nrow(points)

  if (is.null(ideal.point)) {
    ideal.point = approximateIdealPoint(points, ref.points)
  }
  if (is.null(nadir.point)) {
    nadir.point = approximateNadirPoint(points, ref.points)
  }

  assertSameDimensions(points, ref.points, ideal.point, nadir.point)

  if (is.null(lambda)) {
    lambda = determineLambdaByDimension(n.obj)
  }
  lambda = convertInteger(lambda)

  ind.points = .Call("computeRIndicatorC", points, ideal.point, nadir.point, lambda, utility)
  ind.ref.points = .Call("computeRIndicatorC", ref.points, ideal.point, nadir.point, lambda, utility)

  ind = aggregator(ind.points, ind.ref.points)

  return (ind)
}

determineLambdaByDimension = function(n.obj) {
  if (n.obj == 2) {
    500L
  } else if (n.obj == 3) {
    30L
  } else if (n.obj == 4) {
    12L
  } else if (n.obj == 5) {
    8L
  } else {
    3L
  }
}
