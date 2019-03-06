#' @title
#' Polynomial mutation.
#'
#' @description
#' Performs an polynomial mutation as used in the SMS-EMOA algorithm.
#'
#' @param ind [\code{numeric}]\cr
#'   Numeric vector / individual to mutate.
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation of each gene.
#'   Default is 0.2
#' @param eta [\code{numeric(1)}\cr
#'   Distance parameter of the mutation distribution.
#'   Default is 10.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#'   Must have the same length as \code{ind}.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#'   Must have the same length as \code{ind}.
#' @return [\code{numeric}]
#' @family mutators
#' @export
mutPolynomial = makeMutator(
  mutator = function(ind, p = 0.2, eta = 10, lower, upper) {
    checkNumericMutatorArguments(ind, lower, upper, "mutPolynomial")
    checkmate::assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
    checkmate::assertNumber(eta, lower = 1, finite = TRUE, na.ok = FALSE)

    child = .Call("polynomialMutationC", ind, as.numeric(lower), as.numeric(upper), p, eta)
    return(child)
  },
  supported = "float")
