#' @title
#' Dominance relation check.
#'
#' @description
#' Check if a vector dominates another (\code{dominates}) or is
#' dominated by another (\code{isDominated}). There are corresponding infix
#' operators \code{dominates} and \code{isDominatedBy}.
#'
#' @keywords optimize
#'
#' @param x [\code{numeric}]\cr
#'   First vector.
#' @param y [\code{numeric}]\cr
#'   Second vector.
#' @return [\code{logical(1)}]
#' @rdname dominates
#' @export
dominates = function(x, y) {
  stopifnot(length(x) == length(y))
  return(all(x <= y) && any(x < y))
}

#' @rdname dominates
#' @export
isDominated = function(x, y) {
  return(dominates(y, x))
}

#' @rdname dominates
#' @export
`%dominates%` = function(x, y) {
  return(dominates(x, y))
}

#' @rdname dominates
#' @export
`%isDominatedBy%` = function(x, y) {
  return(dominates(y, x))
}

#' @title
#' Check for pareto dominance.
#'
#' @description
#' These functions take a numeric matrix \code{x} where each column corresponds to
#' a point and return a logical vector. The i-th position of the latter is
#' \code{TRUE} if the i-th point is dominated by at least one other point for
#' \code{dominated} and \code{FALSE} for \code{nonDominated}.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#'   Numeric (d x n) matrix where d is the number of objectives and n is the
#'   number of points.
#' @return [\code{logical}]
#' @rdname dominated
#' @export
dominated = function(x) {
  assertMatrix(x, min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  return(.Call("dominatedC", x))
}

#' @rdname dominated
#' @export
nondominated = function(x) {
  return(!dominated(x))
}

#' @title
#' Determine which points of a set are (non)dominated.
#'
#' @description
#' Given a matrix with one point per column \code{which.dominated} returns the
#' column numbers of the dominated points and \code{which.nondominated} the column
#' numbers of the nondominated points. Function \code{isMaximallyDominated} returns
#' a logical vector with \code{TRUE} for each point which is located on the last
#' non-domination level.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#'   Numeric (n x d) matrix where n is the number of points and d is the number
#'   of objectives.
#' @return [\code{integer}]
#' @examples
#'   data(mtcars)
#'   # assume we want to maximize horsepower and minimize gas consumption
#'   cars = mtcars[, c("mpg", "hp")]
#'   cars$hp = -cars$hp
#'   idxs = which.nondominated(as.matrix(cars))
#'   print(mtcars[idxs, ])
#' @rdname which.dominated
#' @export
which.dominated = function(x) {
  return(which(dominated(x)))
}

#' @rdname which.dominated
#' @export
which.nondominated = function(x) {
  return(which(!dominated(x)))
}

#' @rdname which.dominated
#' @export
isMaximallyDominated = function(x) {
  assertMatrix(x, min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  ranks = doNondominatedSorting(x)$ranks
  return(ranks == max(ranks))
}

#' @title
#' Check if one set is better than another.
#'
#' @description
#' The function checks, whether each points of the second set of points
#' is dominated by at least one point from the first set.
#'
#' @param x [\code{matrix}]\cr
#'   First set of points.
#' @param y [\code{matrix}]\cr
#'   Second set of points.
#' @return [\code{logical(1)}]
#' @export
setDominates = function(x, y) {
  n1 = ncol(x)
  n2 = ncol(y)
  z = cbind(x, y)

  dom = dominated(z)
  all(dom[(n1 + 1L):(n1 + n2)])
}
