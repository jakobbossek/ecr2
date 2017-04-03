#' @title
#' Scramble mutator.
#'
#' @description
#' The Scramble mutation operator selects two positions within the chromosome at
#' random and randomly intermixes the subsequence between these positions.
#'
#' @param ind [\code{integer}]\cr
#'   Permutation of integers, i.e., vector of integer values.
#' @return [\code{integer}]
#' @family mutators
#' @export
mutScramble = makeMutator(
  mutator = function(ind) {
    n = length(ind)
    # here we select two distinct positions ...
    idx = sample(seq(n), size = 2L, replace = FALSE)
    # and therefore we do not have the problem to call sample with
    # a single number as parameter x
    ind[idx[1]:idx[2]] = sample(ind[idx[2]:idx[1]])
    return(ind)
  },
  supported = "permutation")
