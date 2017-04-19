#' @title
#' Swap mutator.
#'
#' @description
#' Chooses two positions at random and swaps the genes.
#'
#' @param ind [\code{integer}]\cr
#'   Permutation of integers, i.e., vector of integer values.
#' @return [\code{integer}]
#' @family mutators
#' @export
mutSwap = makeMutator(
  mutator = function(ind) {
    n = length(ind)
    pos = sample(1:n, size = 2L)
    pos1 = pos[1L]
    pos2 = pos[2L]
    tmp = ind[pos1]
    ind[pos1] = ind[pos2]
    ind[pos2] = tmp
    return(ind)
  },
  supported = "permutation")
