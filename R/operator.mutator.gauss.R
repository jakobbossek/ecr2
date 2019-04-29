#' @title
#' Gaussian mutator.
#'
#' @description
#' Default Gaussian mutation operator known from Evolutionary Algorithms.
#' This mutator is applicable only for \code{representation="float"}. Given
#' an individual \eqn{\mathbf{x} \in R^l} this mutator adds a Gaussian
#' distributed random value to each component of \eqn{\mathbf{x}}, i.~e.,
#' \eqn{\tilde{\mathbf{x}}_i = \mathbf{x}_i + \sigma \mathcal{N}(0, 1)}.
#' 
#' @references 
#' [1] Beyer, Hans-Georg & Schwefel, Hans-Paul (2002). Evolution strategies. 
#' Kluwer Academic Publishers.
#' 
#' [2] Mateo, P. M. & Alberto, I. (2011). A mutation operator based
#' on a Pareto ranking for multi-objective evolutionary algorithms. 
#' Springer Science+Business Meda. 57.
#'
#' @param ind [\code{numeric}]\cr
#'   Numeric vector / individual to mutate.
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation for the gauss mutation operator.
#' @param sdev [\code{numeric(1)}\cr
#'   Standard deviance of the Gauss mutation, i. e., the mutation strength.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{numeric}]
#' @family mutators
#' @export
mutGauss = makeMutator(
  mutator = function(ind, p = 1L, sdev = 0.05, lower, upper) {
    checkNumericMutatorArguments(ind, lower, upper, "mutGauss")
    checkmate::assertNumber(p, lower = 0, finite = TRUE, na.ok = FALSE)
    checkmate::assertNumber(sdev, lower = 0, finite = TRUE, na.ok = FALSE)

    n = length(ind)
    mut.idx = runif(n) < p
    mut.noise = rnorm(sum(mut.idx), mean = 0, sd = sdev)
    ind[mut.idx] = ind[mut.idx] + mut.noise
    # correct bounds
    ind = pmin(pmax(lower, ind), upper)
    return(ind)
  },
  supported = "float")
