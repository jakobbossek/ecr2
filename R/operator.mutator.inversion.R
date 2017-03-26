#' @title
#' Generator for the Inversion mutation operator.
#'
#' @description
#' The Inversion mutation operator selects two positions within the chromosome at
#' random and inverts this sub-permutation.
#'
#' @return [\code{ecr2_mutator}]
#' @export
setupInversionMutator = function() {
  mutator = function(ind, par.list) {
    n = length(ind)
    idx = sample(seq(n), size = 2L)
    ind[idx[1]:idx[2]] = ind[idx[2]:idx[1]]
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    supported = "permutation"
  )
}
