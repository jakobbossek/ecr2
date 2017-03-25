#' @title
#' Generator of the Gaussian mutation operator.
#'
#' @description
#' Default Gaussian mutation operator known from Evolutionary Algorithms.
#' This mutator is applicable only for \code{representation="float"}. Given
#' an individual \eqn{\mathbf{x} \in R^l} this mutator adds a Gaussian
#' distributed random value to each component of \eqn{\mathbf{x}}, i.~e.,
#' \eqn{\tilde{\mathbf{x}}_i = \mathbf{x}_i + \sigma \mathcal{N}(0, 1)}.
#'
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation for the gauss mutation operator.
#' @param sdev [\code{numeric(1)}\cr
#'   Standard deviance of the Gauss mutation, i. e., the mutation strength.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupGaussMutator = function(p = 1L, sdev = 0.05, lower, upper) {
  assertNumber(p, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(sdev, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  if (length(lower) != length(upper)) {
    stopf("Gauss mutator: length of lower and upper bounds need to be equal!")
  }

  force(p)
  force(sdev)
  force(lower)
  force(upper)

  mutator = function(ind, par.list) {
    n = length(ind)
    mut.idx = runif(n) < p
    mut.noise = rnorm(sum(mut.idx), mean = 0, sd = sdev)
    ind[mut.idx] = ind[mut.idx] + mut.noise
    # correct bounds
    ind = pmin(pmax(lower, ind), upper)
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Gauss mutator",
    description = "Adds gaussian noise to each gene",
    supported = "float",
    params = list(p = p, sdev = sdev)
  )
}
