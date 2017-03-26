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
#' This functions takes a numeric matrix \code{x} where each column corresponds to
#' a point and returns a logical vector. The i-th position of the latter is
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
  assertMatrix(x, min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  return(.Call("dominated", x))
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
#' Simple wrapper functions around \code{\link{dominated}}. Given a matrix with one
#' point per column \code{which.dominated} returns the row numbers of the dominated
#' points and \code{which.nondominated} the column numbers of the nondominated points.
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

#' @title
#' Plot Pareto-front.
#'
#' @description Plots a scatterplot of non-dominated points in the objective space
#' utilizing the \pkg{ggplot2} package. The function returns a \code{ggplot} object
#' which can be furhter modified via additional ggplot layers.
#' If the passed object is a \code{data.frame}, each line is considered to contain
#' the fitness values of one individual. Contrary, if a matrix is passed, it is
#' considered to be passed in ecr2 format, i.e., each column corresponds to one
#' individual. The matrix is then transposed and converted to a \code{data.frame}.
#'
#' @note At the moment only two-dimensional objective spaces are supported.
#'
#' @param x [\code{matrix} | \code{data.frame}]\cr
#'   Object which contains the non-dominated points.
#' @param obj.names [\code{character}]\cr
#'   Optional objectives names.
#' @return [\code{ggplot}] \pkg{ggplot} object.
#' @examples
#' matrix
#' @export
plotFront = function(x, obj.names = NULL) {
  UseMethod("plotFront")
}

#' @export
plotFront.matrix = function(x, obj.names = NULL) {
  if (nrow(x) != 2L)
    stopf("plotFront: only biobjective spaces supported.")
  df = as.data.frame(t(x))
  names(df) = NULL
  plotFront(df, obj.names = obj.names)
}

#' @export
plotFront.data.frame = function(x, obj.names = NULL) {
  if (ncol(x) != 2L)
    stopf("plotFront: only biobjective spaces supported.")
  n = nrow(x)
  idx.nondominated = nondominated(t(as.matrix(x)))
  n.nondominated = sum(idx.nondominated)
  x = x[idx.nondominated, , drop = FALSE]
  assertCharacter(obj.names, len = ncol(x), any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  if (is.null(names(x))) {
    obj.names = if (!is.null(obj.names)) obj.names else paste0("f", BBmisc::seq_col(x))
    names(x) = obj.names
  }
  ns = names(x)
  BBmisc::requirePackages("ggplot2", why = "ecr2::plotFront")
  pl = ggplot(x, aes_string(x = ns[1L], y = ns[2L])) + geom_point()
  pl = pl + ggtitle(sprintf("Fraction of nondominated points: %.2f", n.nondominated / n))
  return(pl)
}
