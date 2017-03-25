#' @title
#' Generator of the polynomial mutation operator.
#'
#' @description
#' Performs an polynomial mutation as used in the SMS-EMOA algorithm.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation of each gene.
#' @param eta [\code{numeric(1)}\cr
#'   Distance parameter of the mutation distribution.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupPolynomialMutator = function(p = 0.2, eta = 10, lower, upper) {
  assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(eta, lower = 1, finite = TRUE, na.ok = FALSE)
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  if (length(lower) != length(upper)) {
    stopf("Polynomial mutator: length of lower and upper bounds need to be equal!")
  }

  force(p)
  force(eta)
  force(lower)
  force(upper)

  mutator = function(ind, par.list) {
    child = .Call("polynomialMutationC", ind, as.numeric(lower), as.numeric(upper), p, eta)
    return(child)
  }

  makeMutator(
    mutator = mutator,
    name = "Polynomial mutation operator",
    description = "Apply polynomial mutation to an individual.",
    supported = "float",
    params = list(p = p, eta = eta)
  )
}
