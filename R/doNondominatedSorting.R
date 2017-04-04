#' @title
#' Fast non-dominated sorting algorithm.
#'
#' @description
#' Fast non-dominated sorting algorithm proposed by Deb. Non-dominated sorting
#' expects a set of points and returns a
#' set of non-dominated fronts. In short words this is done as follows: the
#' non-dominated points of the entire set are determined and assigned rank 1.
#' Afterwards all points with the current rank are removed, the rank is increased
#' by one and the procedure starts again. This is done until the set is empty, i.~e.,
#' each point is assigned a rank.
#'
#' @note
#' This procedure is the key survival selection of the famous NSGA-II multi-objective
#' evolutionary algorithm (see \code{\link{nsga2}}).
#'
#' @references
#' [1] Deb, K., Pratap, A., and Agarwal, S. A Fast and Elitist Multiobjective Genetic
#' Algorithm: NSGA-II. IEEE Transactions on Evolutionary Computation, 6 (8) (2002),
#' 182-197.
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix of points. Each column contains one point.
#' @return [\code{list}]
#'   List with the following components
#'   \describe{
#'     \item{ranks}{Integer vector of ranks of length \code{ncol(x)}. The higher
#'     the rank, the higher the domination front the corresponding point is
#'     located on.}
#'     \item{dom.counter}{Integer vector of length \code{ncol(x)}. The i-th element
#'     is the domination number of the i-th point.}
#'   }
#' @export
doNondominatedSorting = function(x) {
  assertMatrix(x, min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE, mode = "numeric")
  return(.Call("doNondominatedSortingC", x, PACKAGE = "ecr"))
}

# Old pure R implementation
doNondominatedSortingR = function(x) { # nocov start
  # initialize domination front wrapper
  fronts = list()
  fronts[[1L]] = list()

  n = ncol(x)
  dom.counter = integer(n)
  ranks = integer(n)
  dom.els = vector(mode = "list", length = n)

  # compute domination numbers and pareto front
  for (i in seq.int(n)) {
    for (j in seq.int(n)) {
      if (dominates(x[, i], x[, j])) {
        dom.els[[i]] = c(dom.els[[i]], j)
      } else if (isDominated(x[, i], x[, j])) {
        dom.counter[i] = dom.counter[i] + 1L
      }
    }
    # in this case point x_i belongs to the pareto front, i.e., domination layer 1
    if (dom.counter[i] == 0L) {
      ranks[i] = 1L
      fronts[[1L]] = c(fronts[[1L]], i)
    }
  }

  # make a copy of the dominations number since we are going to modify these
  # in the next lines, but also want to return them
  dom.counter2 = dom.counter

  # now compute the remaining domination fronts
  k = 1L
  while (length(fronts[[k]]) > 0L) {
    front2 = list()
    for (i in fronts[[k]]) {
      for (j in dom.els[[i]]) {
        dom.counter[j] = dom.counter[j] - 1L
        if (dom.counter[j] == 0L) {
          ranks[j] = k + 1L
          front2 = c(front2, j)
        }
      }
    }
    k = k + 1L
    fronts[[k]] = front2
  }

  return(
    list(
      ranks = ranks,
      dom.counter = dom.counter2 # assign the unmodified version
    )
  )
} # nocov end
