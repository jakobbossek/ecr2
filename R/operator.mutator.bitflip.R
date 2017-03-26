#' @title
#' Generator of the Bitplip mutation operator.
#'
#' @description
#' This operator works only on binary representation and flips each bit
#' with a given probability \eqn{p \in (0, 1)}. Usually it is recommended to
#' set \eqn{p = \frac{1}{n}} where \eqn{n} is the number of bits in the
#' representation.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability to flip a single bit.
#'   Default is \code{0.1}.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupBitflipMutator = function(p = 0.1) {
  assertNumber(p, lower = 0, upper = 1)
  force(p)

  mutator = function(ind, par.list) {
    n = length(ind)
    mut.idx = runif(n) < p
    ind[mut.idx] = 1 - ind[mut.idx]
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    supported = "binary"
  )
}

mutBitflip = function(ind, p = 0.1) {
  n = length(ind)
  mut.idx = runif(n) < p
  if (length(mut.idx) > 0L)
    ind[mut.idx] = 1 - ind[mut.idx]
  return(ind)
}
