#' @title
#' Generator for the Swap mutation operator.
#'
#' @description
#' Chooses two positions at random and swaps the genes.
#'
#' @return [\code{ecr2_mutator}]
#' @family mutators
#' @export
setupSwapMutator = function() {
  mutator = function(ind, par.list) {
    n = length(ind)
    pos = sample(1:n, size = 2L)
    pos1 = pos[1L]
    pos2 = pos[2L]
    tmp = ind[pos1]
    ind[pos1] = ind[pos2]
    ind[pos2] = tmp
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    supported = "permutation"
  )
}
