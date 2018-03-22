#' @title Statistical tests of EMOA indicators.
#'
#' @description Given a data.frame as returned by \code{\link{computeIndicators}},
#' the function performs pairwise tests (non-parameteric Wilcoxon sum test at the
#' moment) for location differences in the indicator distributions for all
#' algorithms. Multiple testing issues are handled internally as is the \dQuote{direction}
#' of the indicators (e.g., the unary hypervolume indicator shall be minimized
#' while the number of non-dominated solutions is to be maximized).
#'
#' @references
#' [1] Knowles, J., Thiele, L., & Zitzler, E. (2006). A Tutorial on the Performance Assessment
#' of Stochastic Multiobjective Optimizers. Retrieved from https://sop.tik.ee.ethz.ch/KTZ2005a.pdf
#'
#' @param inds [\code{data.frame}]\cr
#'   Input data frame (return value of \code{\link{computeIndicators}}).
#' @param ind.names [\code{character}]\cr
#'   Optional character vector of indicators (column names of \code{inds}) which
#'   shall be considered in tests.
#'   Defaults to all indicators.
#' @param alpha [\code{numeric(1)}]\cr
#'   Siginificance level for statistical tests.
#'   Default is 0.05.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{list}] Named list of lists of matrizes. The names of the top level
#'   are the names of the problems (\code{unique(inds$prob)}), the names
#'   on the second level are the names of the considered indicators (\code{ind.names}).
#'   Each component of the matrizes on the bottom level contains the adjusted
#'   \eqn{p}-values of the corresponding location test of the indicator for two
#'   algorithms (row- and column names of the matrix).
#' @family EMOA performance assessment tools
#' @export
applyStatisticalTests = function(inds, ind.names, alpha = 0.05, ...) {
  assertSubset(ind.names, choices = colnames(inds))
  assertNumber(alpha, lower = 0.0000001, upper = 1)

  probs = unique(inds$prob)
  algos = unique(inds$algorithm)

  n.probs = length(probs)
  n.algos = length(algos)

  res = list()
  for (prob in probs) {
    res[[prob]] = list()
    for (ind.name in ind.names) {
      p.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
      rownames(p.mat) = colnames(p.mat) = algos
      for (i in 1:n.algos) {
        for (j in 1:n.algos) {
          ind.algo.i = inds[inds$prob == prob & inds$algorithm == algos[i], ind.name]
          ind.algo.j = inds[inds$prob == prob & inds$algorithm == algos[j], ind.name]
          p.mat[i, j] = wilcox.test(x = ind.algo.i, y = ind.algo.j, alternative = "greater")$p.value
        }
      }
      diag(p.mat) = NA
      res[[prob]][[ind.name]] = p.mat
    }
  }

  res = BBmisc::setAttribute(res, "alpha", alpha)

  # catf("=============")
  # print(attr(inds, "unary.inds"))

  unary.inds.names = sapply(attr(inds, "unary.inds"), function(ind.fun) {
    attr(ind.fun$fun, "name")
  })

  unary.inds = attr(inds, "unary.inds")
  names(unary.inds) = unary.inds.names

  res = BBmisc::setAttribute(res, "unary.inds", unary.inds)
  return(res)
}
