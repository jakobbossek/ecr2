#' @title
#' Bitplip mutator.
#'
#' @description
#' This operator works only on binary representation and flips each bit
#' with a given probability \eqn{p \in (0, 1)}. Usually it is recommended to
#' set \eqn{p = \frac{1}{n}} where \eqn{n} is the number of bits in the
#' representation.
#'
#' @param ind [\code{binary}]\cr
#'   Binary vector, i.e., vector with elements 0 and 1 only.
#' @param p [\code{numeric(1)}]\cr
#'   Probability to flip a single bit.
#'   Default is \code{0.1}.
#' @return [\code{binary}]
#' @family mutators
#' @export
mutBitflip = makeMutator(
  mutator = function(ind, p = 0.1) {
    assertNumber(p, lower = 0, upper = 1)
    n = length(ind)
    mut.idx = runif(n) < p
    ind[mut.idx] = 1 - ind[mut.idx]
    return(ind)
  },
  supported = "binary")
