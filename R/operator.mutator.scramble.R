#' @title
#' Generator for the Scramble mutation operator.
#'
#' @description
#' The Scramble mutation operator selects two positions within the chromosome at
#' random and randomly intermixes the subsequence between these positions.
#'
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupScrambleMutator = function() {
  mutator = function(ind, task, control) {
    n.params = length(ind)
    # here we select two distinct positions ...
    idx = sample(seq(n.params), size = 2L, replace = FALSE)
    # and therefore we do not have the problem to call sample with
    # a single number as parameter x
    ind[idx[1]:idx[2]] = sample(ind[idx[2]:idx[1]])
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Scramble mutator",
    description = "Selects two positions at random and intermixes the subsequence.",
    supported = "permutation"
  )
}
