#' @title
#' Population generators
#'
#' @description Utility functions to build a set of individuals. The function
#' \code{gen} expects an R expression and a number n in order to create a list
#' of n individuals based on the given expression. Functions \code{genBin},
#' \code{genPerm} and \code{genReal} are shortcuts for initializing populations
#' of binary strings, permutations or real-valued vectors respectively.
#'
#' @param expr [R expression]\cr
#'   Expression to generate a single individual.
#' @param n [\code{integer(1)}]\cr
#'   Number of individuals to create.
#' @template arg_n_dim
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space in case
#'   of float encoding.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space in case
#'   of float encoding.
#' @return [\code{list}]
#' @family generators
#' @rdname generators
#' @name generators
#' @export
gen = function(expr, n) {
  sapply(integer(n), eval.parent(substitute(function(...) expr)), simplify = FALSE)
}

#' @rdname generators
#' @export
genBin = function(n, n.dim) {
  # FIXME: check how to treat expr unevaluated
  #replicate(n, sample(c(0L, 1L), size = m, replace = TRUE), simplify = TRUE)
  #lapply(seq_len(n), function(i) sample(c(0, 1), size = m, replace = TRUE))
  gen(sample(c(0, 1), size = n.dim, replace = TRUE), n)
}

#' @rdname generators
#' @export
genPerm = function(n, n.dim) {
  gen(sample(seq_len(n.dim)), n)
}

#' @rdname generators
#' @export
genReal = function(n, n.dim, lower, upper) {
  #FIXME: set lower to -min representable and upper
  if (length(lower) == 1L)
    lower = rep(lower, n.dim)
  if (length(upper) == 1L)
    upper = rep(upper, n.dim)

  lapply(seq_len(n), function(i) {
    ind = sapply(seq_len(n.dim), function(j) {
      runif(1L, min = lower[j], max = upper[j])
    })
    return(ind)
  })
}
