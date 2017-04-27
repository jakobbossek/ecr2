#' @title
#' Uniform crossover recombinator.
#'
#' @description
#' Produces two child individuals. The i-th gene is from parent1 with probability
#' \code{p} and from parent2 with probability \code{1-p}.
#'
#' @param inds [\code{list}]\cr
#'   Parents, i.e., list of exactly two numeric or binary vectors of equal length.
#' @param p [\code{numeric(1)}]\cr
#'   Probability to select gene from parent1.
#' @return [\code{list}]
#' @family recombinators
#' @export
recUnifCrossover = makeRecombinator(
  recombinator = function(inds, p = 0.5) {
    n = length(inds[[1L]])

    mask = (runif(n) < p)

    # extract parents
    parent1 = inds[[1L]]
    parent2 = inds[[2L]]

    # back part from other parent
    child1 = parent1
    child2 = parent2

    # front part from "their" parent
    child1[mask] = parent2[mask]
    child2[mask] = parent1[mask]

    return(wrapChildren(child1, child2))
  },
  n.parents = 2L,
  supported = c("float", "binary"),
  n.children = 2L)
