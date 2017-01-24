#' @title
#' Helper function to build initial population.
#'
#' @description
#' Generates the initial population of an EA using the corresponding population
#' generator of the ecr control object. Optionally a set of initial solutions
#' can be passed.
#'
#' @template arg_mu
#' @template arg_control
#' @template arg_initial_solutions
#' @param ... [any]\cr
#'   Further parameters passed to population generator.
#' @return [\code{ecr_population}]
#' @export
initPopulation = function(mu, control, initial.solutions = NULL, ...) {
  n.to.generate = mu
  n.initial = 0L
  if (!is.null(initial.solutions)) {
    assertList(initial.solutions)
    n.initial = length(initial.solutions)
    if (n.initial > mu) {
      stopf("Size of initial population (=%i) exceeds the specified population size %i.",
        n.initial, mu)
    } else if (n.initial == mu) {
      return(initial.solutions)
    }
  }
  generateFun = control$generate
  if (is.null(generateFun))
    stopf("You need to set a generator in case a) no initial population is provided or b)
  the initial population is smaller than mu.")
  gen.solutions = generateFun(mu - n.initial, ...)
  if (n.initial > 0L) {
    return(c(initial.solutions, gen.solutions))
  }
  return(gen.solutions)
}
