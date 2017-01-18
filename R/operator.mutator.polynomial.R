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
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupPolynomialMutator = function(p = 0.2, eta = 10) {
  assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(eta, lower = 1, finite = TRUE, na.ok = FALSE)

  force(p)
  force(eta)

  mutator = function(ind, task, control) {
    par.set = task$par.set
    lower = getLower(par.set)
    upper = getUpper(par.set)
    child = .Call("polynomialMutationC", ind, lower, upper, p, eta)
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
