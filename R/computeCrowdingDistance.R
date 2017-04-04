#' @title
#' Compute the crowding distance of a set of points.
#'
#' @description
#' The crowding distance is a measure of spread of solutions in the
#' approximation of the Pareto front. It is used, e.g., in the NSGA-II algorithm
#' as a second selection criterion.
#'
#' @references
#' K. Deb, A. Pratap, S. Agarwal, T. Meyarivan, A fast and elitist
#' multiobjective genetic algorithm: NSGA-II, IEEE Transactions on Evolutionary
#' Computation In Evolutionary Computation, IEEE Transactions on, Vol. 6, No. 2.
#' (07 April 2002), pp. 182-197, doi:10.1109/4235.996017
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix with each column representing a point.
#' @return [\code{numeric}] Vector of crowding distance values.
#' @export
computeCrowdingDistance = function(x) {
  assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  return(.Call("computeCrowdingDistanceC", x, PACKAGE = "ecr"))
}

# Old pure R function
computeCrowdingDistanceR = function(x) {
  assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)

  n = ncol(x)
  d = nrow(x)
  cds = numeric(n)

  for (i in seq.int(d)) {
    # get the order of the points when sorted according to the i-th objective
    ord = order(x[i, ])
    # set the extreme values to Inf
    cds[ord[1]] = Inf
    cds[ord[n]] = Inf
    # update the remaining crowding numbers
    if (n > 2L) {
      for (j in 2:(n - 1L)) {
        cds[ord[j]] = cds[ord[j]] + (x[i, ord[j + 1L]] - x[i, ord[j - 1L]])
      }
    }
  }
  return(cds)
}
