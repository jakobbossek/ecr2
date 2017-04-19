#' @title
#' One-point crossover recombinator.
#'
#' @description
#' The one-point crossover recombinator is defined for float and binary
#' representations. Given two real-valued/binary vectors of length n, the
#' selector samples a random position i between 1 and n-1. In the next step
#' it creates two children. The first part of the first child contains of the
#' subvector from position 1 to position i of the first parent, the second part
#' from position i+1 to n is taken from the second parent. The second child
#' is build analogously.
#' If the parents are list of real-valued/binary vectors, the procedure described
#' above is applied to each element of the list.
#'
#' @param inds [\code{list}]\cr
#'   Parents, i.e., list of exactly two numeric or binary vectors of equal length.
#' @return [\code{list}]
#' @family recombinators
#' @export
recCrossover = makeRecombinator(
  recombinator = function(inds) {
    n = length(inds[[1L]])

    # recombinate sub genes
    parent1 = inds[[1L]]
    parent2 = inds[[2L]]

    idx = sample(seq(n - 1), size = 1L)
    # back part from other parent
    child1 = parent2
    child2 = parent1
    # front part from "their" parent
    child1[1:idx] = parent1[1:idx]
    child2[1:idx] = parent2[1:idx]

    return(wrapChildren(child1, child2))
  },
  n.parents = 2L,
  supported = c("float", "binary"),
  n.children = 2L)
