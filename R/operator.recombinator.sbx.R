#' @title
#' Generator of the Simulated Binary Crossover (SBX) recombinator.
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
#' @param eta [\code{numeric(1)}]\cr
#'   Parameter eta, i.e., the distance parameters of the crossover distribution.
#' @param p [\code{numeric(1)}]\cr
#'   Crossover probability for each gene. Default is \code{1.0}.
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupSBXRecombinator = function(eta = 5, p = 1.0) {
  assertNumber(eta, lower = 1, na.ok = FALSE)
  assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)

  force(eta)
  force(p)

  recombinator = function(inds, task, control) {
    par.set = task$par.set
    n.params = getParamLengths(par.set)
    lower = getLower(par.set)
    upper = getUpper(par.set)

    # convert parents to d x 2 matrix for C
    inds = do.call(cbind, inds)

    # SBX produces two children
    children = .Call("simulatedBinaryCrossoverC", inds, as.numeric(lower), as.numeric(upper), p, eta)

    return(wrapChildren(children[, 1L], children[, 2L]))
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "Simulated Binary Crossover (SBX) recombinator",
    description = "Performs simulated binary crossover.",
    n.parents = 2L,
    supported = c("float"),
    params = list(p = p, eta = eta),
    n.children = 2L
  )
}
