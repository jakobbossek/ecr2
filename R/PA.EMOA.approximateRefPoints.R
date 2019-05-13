#' @title Helper function to estimate reference points.
#'
#' @description E.g., for calculation of dominated hypervolume.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with the required structure, i.e. the data frame must contain a problem column "prob" as well as objective column(s).
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names of the objective functions.
#'   Default is \code{c("f1", "f2")}, i.e., the bi-objective case is assumed.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset added to reference points.
#'   Default is \code{0}.
#' @param as.df [\code{logical(1)}]\cr
#'   Should a data.frame be returned?
#'   Default is \code{FALSE}. In this case a named list is returned.
#' @return [\code{list} | \code{data.frame}]
#' @family EMOA performance assessment tools
#' @export
approximateRefPoints = function(df, obj.cols = c("f1", "f2"), offset = 0, as.df = FALSE) {
  # split by prob(lem)
  ref.points = by(df, list(df$prob), function(x) {
    # get data points
    pf.approx = x[, obj.cols, drop = FALSE]
    # compute reference point
    ref.point = apply(pf.approx, 2L, max) + offset
    # return a list with component prob
    res = list()
    res[[as.character(x$prob[1L])]] = ref.point
    return(res)
  })
  # drop "by" class and attributes
  attr(ref.points, "call") = NULL
  ref.points = unclass(ref.points)
  # eventually convert to data.frame
  if (as.df) {
    probs = names(ref.points)
    ref.points = as.data.frame(do.call(rbind, unname(ref.points)))
    ref.points$prob = probs
  }
  return(ref.points)
}
