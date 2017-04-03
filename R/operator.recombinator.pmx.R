#' @title
#' Partially-Mapped-Crossover (PMX) recombinator.
#'
#' @description
#' This recombination operator is specifically designed for permutations.
#' The operators chooses two cut-points at random and generates two offspring
#' as follows: a) copy the subsequence of one parent and b) fill the remaining
#' positions while preserving the order and position of as many genes as possible.
#'
#' @param inds [\code{numeric}]\cr
#'   Parents, i.e., list of exactly two permutations of equal length.
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
recPMX = makeRecombinator(
  recombinator = function(inds) {
    p1 = inds[[1L]]
    p2 = inds[[2L]]
    n = length(p1)

    # select two random positions and bring them in order
    idx = sample(n, 2L)
    i1 = idx[1L]
    i2 = idx[2L]
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
  },
  supported = "permutation",
  n.parents = 2L,
  n.children = 2L)
