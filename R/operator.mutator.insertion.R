#' @title
#' Insertion mutator.
#'
#' @description
#' The Insertion mutation operator selects a position random and inserts it at
#' a random position.
#'
#' @param ind [\code{integer}]\cr
#'   Permutation of integers, i.e., vector of integer values.
#' @return [\code{integer}]
#' @family mutators
#' @export
mutInsertion = makeMutator(
  mutator = function(ind) {
    n = length(ind)
    # select a random position and insert it at another random location
    idx = sample(seq(n), size = 2L)
    # idx[1] is the selected position
    # idx[2] is the destination
    # equality is impossible, since replace = FALSE in sample above
    tmp = ind[idx[1]]
    # determine shift direction
    offset = if (idx[1] < idx[2]) (1) else (-1)
    ind[idx[1]:(idx[2] - offset)] = ind[(idx[1] + offset):idx[2]]
    ind[idx[2]] = tmp
    return(ind)
  },
  supported = "permutation")
