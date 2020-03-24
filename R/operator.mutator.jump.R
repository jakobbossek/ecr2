#' @title
#' Jump mutator.
#'
#' @description
#' The jump mutation operator selects two positions within the chromosome at
#' random, say \eqn{a} and \eqn{b} with \eqn{a < b}. Next, all elements at
#' positions \eqn{b-1, b-2, ..., a} are shifted to the right by one position
#' and finally the element at position \eqn{b} is assigned at position \eqn{a}.
#'
#' @param ind [\code{integer}]\cr
#'   Permutation of integers, i.e., vector of integer values.
#' @return [\code{integer}]
#' @family mutators
#' @export
mutJump = makeMutator(
  mutator = function(ind) {
    idxs = sample(1:length(ind), size = 2L, replace = FALSE)
    a = idxs[1L]
    b = idxs[2L]
    # swap such that a < b
    if (a > b) {
      tmp = a
      a = b
      b = tmp
    }

    # now perform the mutation
    tmp = ind[b]
    ind[(a+1L):b] = ind[a:(b-1L)]
    ind[a] = tmp

    return(ind)
  },
  supported = "permutation")
