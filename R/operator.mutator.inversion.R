#' @title
#' Inversion mutator.
#'
#' @description
#' The Inversion mutation operator selects two positions within the chromosome at
#' random and inverts the elements inbetween.
#'
#' @param ind [\code{integer}]\cr
#'   Permutation of integers, i.e., vector of integer values.
#' @return [\code{integer}]
#' @family mutators
#' @export
mutInversion = makeMutator(
  mutator = function(ind) {
    n = length(ind)
    idx = sample(seq(n), size = 2L)
    ind[idx[1]:idx[2]] = ind[idx[2]:idx[1]]
    return(ind)
  },
  supported = "permutation")
