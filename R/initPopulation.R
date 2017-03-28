#' @title
#' Helper function to build initial population.
#'
#' @description
#' Generates the initial population. Optionally a set of initial solutions
#' can be passed.
#'
#' @template arg_mu
#' @param gen.fun [\code{function}]\cr
#'   Function used to generate initial solutions, e.g., \code{\link{genBin}}.
#' @template arg_initial_solutions
#' @param ... [any]\cr
#'   Further parameters passed to \code{gen.fun}.
#' @return [\code{ecr_population}]
#' @export
initPopulation = function(mu, gen.fun, initial.solutions = NULL, ...) {
  n = asInt(mu, lower = 1L)
  assertFunction(gen.fun)
  n.to.generate = n
  n.initial = 0L
  if (!is.null(initial.solutions)) {
    assertList(initial.solutions)
    n.initial = length(initial.solutions)
    if (n.initial > n) {
      stopf("Size of initial population (=%i) exceeds the specified population size %i.",
        n.initial, n)
    } else if (n.initial == n) {
      return(initial.solutions)
    }
  }
  if (is.null(gen.fun))
    stopf("You need to set a generator in case a) no initial population is provided or b)
  the initial population is smaller than mu.")
  gen.solutions = gen.fun(n - n.initial, ...)
  if (n.initial > 0L) {
    return(c(initial.solutions, gen.solutions))
  }
  return(gen.solutions)
}
