#' @title
#' Generator of the Ordered-Crossover (OX) recombination operator.
#'
#' @description
#' This recombination operator is specifically designed for permutations.
#' The operators chooses two cut-points at random and generates two offspring
#' as follows: a) copy the subsequence of one parent and b) remove the copied
#' node indizes from the entire sequence of the second parent from the sescond
#' cut point and b) fill the remaining gaps with this trimmed sequence.
#'
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupOXRecombinator = function() {
  recombinator = function(inds, task, control) {
    p1 = inds[[1]]
    p2 = inds[[2]]
    n = length(p1)

    # select two random positions and bring them in order
    idx = sample(1:(n-1), 2L)
    cut1 = idx[1]
    cut2 = idx[2]
    if (cut2 < cut1) {
      tmp = cut1
      cut1 = cut2
      cut2 = tmp
    }

    # length of copied subsequence
    m = cut2 - cut1 + 1L

    # number of missing elements
    n.miss = length(p1) - m - 1L

    # copy the subsequence
    c1 = rep(NA, n)
    c2 = rep(NA, n)

    # copy subsequences
    c1[cut1:cut2] = p1[cut1:cut2]
    c2[cut1:cut2] = p2[cut1:cut2]

    # now take "the rest" from the other parent from the second cut points ...
    r1 = p2[(((cut2):(cut2 + n)) %% n) + 1L]
    r2 = p1[(((cut2):(cut2 + n)) %% n) + 1L]

    # ... and remove the already copied stuff
    r1 = setdiff(r1, p1[which(!is.na(c1))])
    r2 = setdiff(r2, p2[which(!is.na(c2))])

    # complete the child sequences
    c1[(cut2:(cut2 + n.miss) %% n) + 1L] = r1
    c2[(cut2:(cut2 + n.miss) %% n) + 1L] = r2

    return(wrapChildren(c1, c2))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "OX recombinator",
    description = "Performs Ordered-Crossover on permutations.",
    supported = "permutation",
    n.parents = 2L,
    n.children = 2L
  )
}
