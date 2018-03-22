#' @title
#' Normalize approximations set(s).
#'
#' @description
#' Normalization is done by subtracting the \code{min.value} for each dimension
#' and dividing by the difference \code{max.value - min.value} for each dimension
#' Certain EMOA indicators require all elements to be strictly positive. Hence, an optional
#' \code{offset} is added to each element which defaults to zero.
#'
#' @note In case a data.frame is passed and a \dQuote{prob} column exists, normalization
#' is performed for each unique element of the \dQuote{prob} column independently (if
#' existent).
#'
#' @param x [\code{matrix} | \code{data.frame}]\cr
#'   Either a numeric \code{matrix} (each column corresponds to a point) or a
#'   \code{data.frame} with columns at least \code{obj.cols}.
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names of the objective functions.
#' @param min.value [\code{numeric}]\cr
#'   Vector of minimal values of length \code{nrow(x)}.
#'   Only relevant if \code{x} is a matrix.
#'   Default is the row-wise minimum of \code{x}.
#' @param max.value [\code{numeric}]\cr
#'   Vector of maximal values of length \code{nrow(x)}.
#'   Only relevant if \code{x} is a matrix.
#'   Default is the row-wise maximum of \code{x}.
#' @param offset [\code{numeric}]\cr
#'   Numeric constant added to each normalized element.
#'   Useful to make all objectives strictly positive, e.g., located in \eqn{[1,2]}.
#' @return [\code{matrix} | \code{data.frame}]
#' @family EMOA performance assessment tools
#' @export
normalize = function(x, obj.cols, min.value = NULL, max.value = NULL, offset = NULL) {
  if (is.matrix(x))
    return(normalizeMatrix(x, min.value = min.value, max.value = max.value, offset = offset))

  #FIXME: export to helper
  if (!all(obj.cols %in% colnames(x)))
    stopf("obj.cols needs to contain valid column names.")

  x.obj = x[, obj.cols, drop = FALSE]
  obj.cols.numeric = sapply(x.obj, is.numeric)
  if (!all(obj.cols.numeric))
    stopf("Only numeric values allowed in obj.cols, but column(s) '%s' %s not numeric!",
      collapse(obj.cols[which(!obj.cols.numeric)], ifelse(sum(!obj.cols.numeric) > 1L, "are", "is")))

  if (is.null(offset))
    offset = rep(0.0, length(obj.cols))

  # add dummy problem col (deleted afterwards)
  has.prob = !is.null(x$prob)
  if (!has.prob)
    x$prob = "Dummy"

  # start normalization
  probs = unique(x$prob)
  normalized.x = lapply(probs, function(prob) {
    tmp = x[x$prob == prob, , drop = FALSE]
    tmp[, obj.cols] = t(normalizeMatrix(t(as.matrix(tmp[, obj.cols])), offset = offset))
    return(tmp)
  })
  x = do.call(rbind, normalized.x)
  # end normalization

  # cleanup
  if (!has.prob)
    x$prob = NULL

  return(x)
}

normalizeMatrix = function(x, min.value = NULL, max.value = NULL, offset = NULL) {
  assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 2L, min.cols = 2L)
  if (is.null(offset))
    offset = rep(0.0, nrow(x))
  if (is.null(min.value))
    min.value = apply(x, 1L, min)
  if (is.null(max.value))
    max.value = apply(x, 1L, max)
  return(((x - min.value) / (max.value - min.value)) + offset)
}
