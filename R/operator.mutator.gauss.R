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
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupGaussMutator = function(p = 1L, sdev = 0.05) {
  assertNumber(p, lower = 0, finite = TRUE, na.ok = FALSE)
  assertNumber(sdev, lower = 0, finite = TRUE, na.ok = FALSE)

  force(p)
  force(sdev)

  mutator = function(ind, ...) {
    args = list(...)
    n = length(ind)
    mut.idx = runif(n) < p
    mut.noise = rnorm(sum(mut.idx), mean = 0, sd = sdev)
    ind[mut.idx] = ind[mut.idx] + mut.noise
    # correct bounds
    ind = pmin(pmax(args$lower, ind), args$upper)
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
