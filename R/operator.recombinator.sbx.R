#' @title
#' Simulated Binary Crossover (SBX) recombinator.
#'
#' @description
#' The Simulated Binary Crossover was first proposed by [1]. It i suited for
#' float representation only and creates two offspring. Given parents \eqn{p_1, p_2}
#' the offspring are generated as \eqn{c_{1/2} = \bar{x} \pm \frac{1}{2}\beta(p_2 - p_1)}
#' where \eqn{\bar{x} = \frac{1}{2}(p_1 + p_2), p_2 > p_1}. This way \eqn{\bar{c} = \bar{x}}
#' is assured.
#'
#' @note
#' This is the default recombination operator used in the NSGA-II EMOA (see \code{\link{nsga2}}).
#'
#' @references
#' [1] Deb, K. and Agrawal, R. B. (1995). Simulated binary crossover for continuous
#' search space. Complex Systems 9(2), 115-148.
#'
#' @param inds [\code{numeric}]\cr
#'   Parents, i.e., list of exactly two numeric vectors of equal length.
#' @param eta [\code{numeric(1)}]\cr
#'   Parameter eta, i.e., the distance parameters of the crossover distribution.
#' @param p [\code{numeric(1)}]\cr
#'   Crossover probability for each gene. Default is \code{1.0}.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
recSBX = makeRecombinator(
  recombinator = function(inds, eta = 5, p = 1.0, lower, upper) {
    assertNumber(eta, lower = 1, na.ok = FALSE)
    assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
    assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
    assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
    if (length(lower) != length(upper)) {
      stopf("Polynomial mutator: length of lower and upper bounds need to be equal!")
    }

    # convert parents to d x 2 matrix for C
    inds = do.call(cbind, inds)

    # SBX produces two children
    children = .Call("simulatedBinaryCrossoverC", inds, lower, upper, p, eta)

    return(wrapChildren(children[, 1L], children[, 2L]))
  },
  n.parents = 2L,
  supported = "float",
  n.children = 2L)
