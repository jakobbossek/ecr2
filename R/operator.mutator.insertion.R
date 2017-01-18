#' @title
#' Generator for the Insertion mutation operator.
#'
#' @description
#' The Insertion mutation operator selects a position random and inserts it at
#' a random position.
#'
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupInsertionMutator = function() {
  mutator = function(ind, task, control) {
    n.params = length(ind)
    # select a random position and insert it at another random location
    idx = sample(seq(n.params), size = 2L)
    # idx[1] is the selected position
    # idx[2] is the destination
    # equality is impossible, since replace = FALSE in sample above
    tmp = ind[idx[1]]
    # determine shift direction
    offset = if (idx[1] < idx[2]) (1) else (-1)
    ind[idx[1]:(idx[2] - offset)] = ind[(idx[1] + offset):idx[2]]
    ind[idx[2]] = tmp
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Insertion mutator",
    description = "Selects two positions at random, places the first position to
    the second and shift the other elements accordingly.",
    supported = "permutation"
  )
}
