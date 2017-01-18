#' @title
#' Generator of the Partially-Mapped-Crossover (PMX) recombination operator.
#'
#' @description
#' This recombination operator is specifically designed for permutations.
#' The operators chooses two cut-points at random and generates two offspring
#' as follows: a) copy the subsequence of one parent and b) fill the remaining
#' positions while preserving the order and position of as many genes as possible.
#'
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupPMXRecombinator = function() {
  recombinator = function(inds, task, control) {
    p1 = inds[[1]]
    p2 = inds[[2]]
    n = length(p1)

    # select two random positions and bring them in order
    idx = sample(n, 2L)
    i1 = idx[1]
    i2 = idx[2]
    if (i2 < i1) {
      tmp = i1
      i1 = i2
      i2 = tmp
    }

    # swap the subsequence
    c1 = rep(NA, n)
    c2 = rep(NA, n)
    c1[i1:i2] = p2[i1:i2]
    c2[i1:i2] = p1[i1:i2]

    # fix conflict maintaining position of genes which are not in conflict
    #FIXME: this is a quick & dirty ugly implementation.
    for (i in setdiff(seq(n), i1:i2)) {
      if (p1[i] %nin% c1[i1:i2]) {
        c1[i] = p1[i]
      } else {
        ins = c2[which(c1 == p1[i])]
        while (ins %in% c1[i1:i2]) {
          ins = c2[which(c1 == ins)]
        }
        c1[i] = ins
      }
      if (p2[i] %nin% c2[i1:i2]) {
        c2[i] = p2[i]
      } else {
        ins = c1[which(c2 == p2[i])]
        while (ins %in% c2[i1:i2]) {
          ins = c1[which(c2 == ins)]
        }
        c2[i] = ins
      }
    }
    return(wrapChildren(c1, c2))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "PMX recombinator",
    description = "Performs partially mapped crossover on permutations.",
    supported = "permutation",
    n.parents = 2L,
    n.children = 2L
  )
}
