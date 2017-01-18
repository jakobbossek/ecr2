#' @title
#' Generator of the one-point crossover recombination operator.
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
#' @param p [\code{numeric(1)}]\cr
#'   Cross over probability to form an offspring. Default is \code{1}.
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupCrossoverRecombinator = function(p = 1) {
  assertNumber(p, lower = 0, upper = 1)
  force(p)

  recombinator = function(inds, task, control) {
    par.set = task$par.set
    n.params = getParamLengths(par.set)
    # we have to make sure, that the gene has length > 1. This should not
    # be the case in pratice use, but it causes errors
    if (min(n.params) <= 1L) {
      stopf("Crossover recombinator requires genes to have length > 1.")
    }

    # do a cross-over or not
    if (runif(1L) >= p) {
      return(wrapChildren(inds[[1]], inds[[2]]))
    }

    # recombinate sub genes
    recombGenes = function(parent1, parent2, n) {
      idx = sample(seq(n - 1), size = 1L)
      # back part from other parent
      child1 = parent2
      child2 = parent1
      # front part from "their" parent
      child1[1:idx] = parent1[1:idx]
      child2[1:idx] = parent2[1:idx]

      return(list(child1, child2))
    }

    # overwrite individuals with offsprings
    if (getParamNr(par.set) == 1L) {
      inds = recombGenes(inds[[1]], inds[[2]], n.params)
    } else {
      for (i in seq(getParamNr(par.set))) {
        children = recombGenes(inds[[1]][[i]], inds[[2]][[i]], n.params[i])
        inds[[1]][[i]] = children[[1]]
        inds[[2]][[i]] = children[[2]]
      }
    }
    # return two offsprings
    return(wrapChildren(inds[[1]], inds[[2]]))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Crossover recombinator",
    description = "Performs classical one-point crossover.",
    n.parents = 2L,
    supported = c("float", "binary"),
    params = list(p = p),
    n.children = 2L
  )
}
