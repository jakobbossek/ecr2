#' @title Statistical tests of EMOA indicators.
#'
#' @description Given a data frame (e.g., returned by \code{\link{computeIndicators}}),
#' the function performs pairwise tests (non-parameteric Wilcoxon sum test at the
#' moment) for location differences in the distributions of all columns
#' for all pairs of algorithms. Multiple testing issues are handled internally.
#' Moreover, if the data frame passed is the result of \code{\link{computeIndicators}}
#' the \dQuote{direction} of the indicators (e.g., the unary hypervolume indicator
#' shall be minimized while the number of non-dominated solutions is to be maximized)
#' is considered as well.
#'
#' @references
#' [1] Knowles, J., Thiele, L., & Zitzler, E. (2006). A Tutorial on the Performance Assessment
#' of Stochastic Multiobjective Optimizers. Retrieved from https://sop.tik.ee.ethz.ch/KTZ2005a.pdf
#'
#' @param df [\code{data.frame}]\cr
#'   Input data frame (e.g., return value of \code{\link{computeIndicators}}).
#' @param cols [\code{character}]\cr
#'   Character vector of column names which shall be considered in tests.
#' @param alpha [\code{numeric(1)}]\cr
#'   Siginificance level for statistical tests.
#'   Default is 0.05.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{list}] Named list of lists of matrizes. The names of the top level
#'   are the names of the problems (\code{unique(df$prob)}), the names
#'   on the second level are the names of the considered columns (\code{cols}).
#'   Each component of the matrizes on the bottom level contains the adjusted
#'   \eqn{p}-values of the corresponding location test of the indicator for two
#'   algorithms (row- and column names of the matrix).
#' @family EMOA performance assessment tools
#' @export
test = function(df, cols, alpha = 0.05, ...) {
  assertDataFrame(df)
  cnames = colnames(df)
  assertSubset(cols, choices = cnames)
  assertNumber(alpha, lower = 0.0000001, upper = 1)

  probs = unique(df$prob)
  algos = unique(df$algorithm)

  n.probs = length(probs)
  n.algos = length(algos)

  res = list()
  for (prob in probs) {
    res[[prob]] = list()
    for (col in cols) {
      p.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
      rownames(p.mat) = colnames(p.mat) = algos
      for (i in 1:n.algos) {
        for (j in 1:n.algos) {
          ind.algo.i = df[df$prob == prob & df$algorithm == algos[i], col]
          if (!is.numeric(ind.algo.i))
            ind.algo.i = ind.algo.i[[col]]
          ind.algo.j = df[df$prob == prob & df$algorithm == algos[j], col]
          if (!is.numeric(ind.algo.j))
            ind.algo.j = ind.algo.j[[col]]
          #FIXME: directions
          p.mat[i, j] = wilcox.test(x = ind.algo.i, y = ind.algo.j, alternative = "greater")$p.value
        }
      }
      diag(p.mat) = NA
      res[[prob]][[col]] = p.mat
    }
  }

  res = BBmisc::setAttribute(res, "alpha", alpha)

  if (BBmisc::hasAttributes(df, "unary.inds")) {
    unary.inds.names = sapply(attr(df, "unary.inds"), function(ind.fun) {
      attr(ind.fun$fun, "name")
    })

    unary.inds = attr(df, "unary.inds")
    names(unary.inds) = unary.inds.names

    res = BBmisc::setAttribute(res, "unary.inds", unary.inds)
  }
  return(res)
}
