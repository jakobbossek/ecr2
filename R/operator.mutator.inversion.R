#' @title
#' Generator for the Inversion mutation operator.
#'
#' @description
#' The Inversion mutation operator selects two positions within the chromosome at
#' random and inverts this sub-permutation.
#'
#' @return [\code{ecr_mutator}]
#' @export
setupInversionMutator = function() {
  mutator = function(ind, task, control) {
    n.params = length(ind)
    idx = sample(seq(n.params), size = 2L)
    ind[idx[1]:idx[2]] = ind[idx[2]:idx[1]]
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Inversion mutator",
    description = "Selects two positions at random and inverts.",
    supported = "permutation"
  )
}
