#' @title Draw scatterplot of Pareto-front approximation
#'
#' @description The function expects a data.frame or a matrix. By default the first
#' 2 or 3 columns/rows are assumed to contain the elements of the approximation sets.
#' Depending on the number of numeric columns (in case of a data.frame) or the
#' number of rows (in case of a matrix) the function internally calls
#' \code{\link{plotScatter2d}} or \code{plotScatter3d}.
#'
#' @param x [\code{matrix} | \code{data.frame}]\cr
#'   Object which contains the approximations sets.
#' @param obj.names [\code{character}]\cr
#'   Optional objectives names.
#'   Default is \code{c("f1", "f2")}.
#' @param minimize [\code{logical}]\cr
#'   Logical vector with ith entry \code{TRUE} if the ith objective shall be minimized.
#'   If a single logical is passed, it is assumed to be valid for each objective.
#'   If the matrix is of type \code{ecr_fitness_matrix} (this is the case if it is
#'   produced by one of ecr2's utility functions, e.g. \code{\link{evaluateFitness}}),
#'   the appended \code{minimize} attribute is the default.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{ggplot}] \pkg{ggplot} object.
#' @family EMOA performance assessment tools
#' @export
plotFront = function(x, obj.names = NULL, minimize = TRUE, ...) {
  UseMethod("plotFront")
}

#' @export
plotFront.matrix = function(x, obj.names = NULL, minimize = TRUE, ...) {
  n.obj = nrow(x)
  if (n.obj > 3L | n.obj < 1L)
    stopf("plotFront: only {2,3}-objective spaces supported.")
  df = as.data.frame(t(x))
  if (is.null(obj.names)) {
    obj.names = paste0("f", seq_len(n.obj))
  }
  names(df) = obj.names
  df$prob = "Prob"
  df$algorithm = "Algo"
  if (n.obj == 2L)
    plotScatter2d(df, ...)
  else
    plotScatter3d(df, ...)
}

#' @export
plotFront.ecr_fitness_matrix = function(x, obj.names = NULL, minimize = attr(x, "minimize"), ...) {
  plotFront.matrix(x, obj.names, minimize = minimize)
}

#' @export
plotFront.data.frame = function(x, obj.names = NULL, minimize = TRUE, ...) {
  n.cols = ncol(x)
  if (n.cols > 3L | n.cols < 1L)
    stopf("plotFront: only {2,3}-objective spaces supported.")

  if (length(minimize) == 1L)
    minimize = rep(minimize, 2L)

  # x$prob =
  # if (n.cols == 2L) {

  # }

  # filter non-dominated points
  # we need to transform here (back) to a matrix and scale the matrix if not all
  # objectives are to be minimized
  xmat = t(as.matrix(x))
  fn.scale = ifelse(xor(minimize, c(TRUE, TRUE)), -1, 1)
  fn.scale = diag(fn.scale)
  xmat = fn.scale %*% xmat
  idx.nondominated = nondominated(xmat)

  n.nondominated = sum(idx.nondominated)
  x = x[idx.nondominated, , drop = FALSE]
  assertCharacter(obj.names, len = ncol(x), any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  if (is.null(names(x))) {
    obj.names = if (!is.null(obj.names)) obj.names else paste0("f", BBmisc::seq_col(x))
    names(x) = obj.names
  }
  ns = names(x)
  dirs = ifelse(minimize, "min", "max")
  BBmisc::requirePackages("ggplot2", why = "ecr::plotFront")
  pl = ggplot(x, aes_string(x = ns[1L], y = ns[2L])) + geom_point()
  pl = pl + xlab(sprintf("%s (-> %s)", ns[1L], dirs[1L]))
  pl = pl + ylab(sprintf("%s (-> %s)", ns[2L], dirs[2L]))
  #pl = pl + ggtitle(sprintf("Fraction of nondominated points: %.2f", n.nondominated / n))
  return(pl)
}
