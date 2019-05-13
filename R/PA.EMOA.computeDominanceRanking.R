#' @title Ranking of approximation sets.
#'
#' @description Ranking is performed by merging all approximation sets over all
#' algorithms and runs per instance. Next, each approximation set \eqn{C} is assigned a
#' rank which is 1 plus the number of approximation sets that are better than
#' \eqn{C}. A set \eqn{D} is better than \eqn{C}, if for each point \eqn{x \in C}{x in C} there
#' exists a point in \eqn{y \in D}{y in D} which weakly dominates \eqn{x}.
#' Thus, each approximation set is reduced to a number -- its rank. This rank distribution
#' may act for first comparrison of multi-objecitve stochastic optimizers.
#' See [1] for more details.
#' This function makes use of \code{\link[parallelMap]{parallelMap}} to
#' parallelize the computation of dominance ranks.
#' @note Since pairwise non-domination checks are performed over all algorithms and
#' algorithm runs this function may take some time if the number of problems, algorithms
#' and/or replications is high.
#'
#' @references
#' [1] Knowles, J., Thiele, L., & Zitzler, E. (2006). A Tutorial on the Performance Assessment
#' of Stochastic Multiobjective Optimizers. Retrieved from https://sop.tik.ee.ethz.ch/KTZ2005a.pdf
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with columns at least \dQuote{prob}, \dQuote{algorithm}, \dQuote{repl} and
#'   column names specified via parameter \code{obj.cols}.
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names in \code{df} which store the objective function values.
#' @return [\code{data.frame}] Reduced \code{df} with columns \dQuote{prob}, \dQuote{algorithm}, \dQuote{repl}
#'   and \dQuote{rank}.
#' @family EMOA performance assessment tools
#' @export
computeDominanceRanking = function(df, obj.cols) {
  assertDataFrame(df)
  assertCharacter(obj.cols, min.len = 2L)

  #warning
  warningf("Note that this may take some time if the number of problems, algorithms and/or replications is high. \n The usage of parallelization is recommended.")
  
  magic = function(x) {
    # get grid the ugly way: now we need to iterate over the rows
    grid = dplyr::group_by_(x, "prob", "algorithm", "repl")
    grid = dplyr::summarize(grid, rank = NA)
    grid = dplyr::ungroup(grid)
    
    n = nrow(grid)
    for (i in 1:n) {
      # Rank is rk(C_i) = 1 + |{C_j in C | C_j <= C_i}| (see Knowles et al.)
      rk = 1L
      cat(".")
      for (j in 1:n) {
        # nothing to do here
        if (i == j)
          next
        approx.a = x[x$algorithm == as.character(grid[i, "algorithm"]) & x$repl == as.integer(grid[i, "repl"]), obj.cols, drop = FALSE]
        approx.b = x[x$algorithm == as.character(grid[j, "algorithm"]) & x$repl == as.integer(grid[j, "repl"]), obj.cols, drop = FALSE]
        rk = rk + as.integer(setDominates(t(approx.b), t(approx.a)))
      }
      grid[i, "rank"] = rk
    }
    return(grid)
  }
  # split by problems and compute rankings
  listofDfs = split(df, df$prob)
  res = parallelLapply(listofDfs, magic, level = "ecr.computeDominanceRanking")
  do.call(rbind, res)
}

