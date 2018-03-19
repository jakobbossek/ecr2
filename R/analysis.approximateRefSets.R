#' @title Helper function to estimate reference sets.
#'
#' @description E.g., for computation of certain EMOA indicators.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with the required structure.
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
#' @export
approximateRefSets = function(df, obj.cols, as.df = FALSE) {
  # split by prob(lem)
  ref.sets = by(df, list(df$prob), function(x) {
    # get data points
    pf.approx = t(as.matrix(x[, obj.cols, drop = FALSE]))
    # compute reference point
    ref.set = pf.approx[, ecr::nondominated(pf.approx), drop = FALSE]
    # return a list with component prob
    res = list()
    res[[x$prob[1L]]] = ref.set
    return(res)
  })
  # drop "by" class and attributes
  attr(ref.sets, "call") = NULL
  ref.sets = unclass(ref.sets)
  # eventually convert to data.frame
  if (as.df) {
    probs = names(ref.sets)
    for (prob in probs) {
      ref.sets[[prob]] = as.data.frame(t(ref.sets[[prob]]))
      ref.sets[[prob]]$prob = prob
    }
    ref.sets = do.call(rbind, unname(ref.sets))
  }
  return(ref.sets)
}
