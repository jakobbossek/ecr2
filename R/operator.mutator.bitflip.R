#' @title
#' Generator of the Bitplip mutation operator.
#'
#' @description
#' This operator works only on binary representation and flips each bit
#' with a given probability \eqn{p \in (0, 1)}. Usually it is recommended to
#' set \eqn{p = \frac{1}{n}} where \eqn{n} is the number of bits in the
#' representation.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability to flip a single bit.
#'   Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupBitFlipMutator = function(p = 0.1) {
  assertNumber(p, lower = 0, upper = 1)

  force(p)

  mutator = function(ind, task, control) {
    n.params = length(ind)
    mutateGene = function(gene, prob) {
      do.mutate = runif(length(gene)) < prob
      gene[do.mutate] = 1 - gene[do.mutate]
      gene
    }

    if (getParamNr(task$par.set) == 1L) {
      ind = mutateGene(ind, p)
    } else {
      ind = lapply(ind, mutateGene, prob = p)
    }

    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Bitflip mutator",
    description = "Flips each bit of the allele with a specific probability.",
    supported = "binary",
    params = list(p = p)
  )
}
