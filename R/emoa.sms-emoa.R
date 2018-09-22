#' @title
#' Implementation of the SMS-EMOA by Emmerich et al.
#'
#' @description
#' Pure R implementation of the SMS-EMOA. This algorithm belongs to the group
#' of indicator based multi-objective evolutionary algorithms. In each generation,
#' the SMS-EMOA selects two parents uniformly at, applies recombination and mutation
#' and finally selects the best subset of individuals among all subsets by maximizing
#' the Hypervolume indicator.
#'
#' @note
#' This helper function hides the regular ecr interface and offers a more
#' R like interface of this state of the art EMOA.
#'
#' @keywords optimize
#'
#' @references
#' Beume, N., Naujoks, B., Emmerich, M., SMS-EMOA: Multiobjective selection based
#' on dominated hypervolume, European Journal of Operational Research, Volume 181,
#' Issue 3, 16 September 2007, Pages 1653-1669.
#'
#' @template arg_fitness_fun
#' @template arg_n_objectives
#' @template arg_n_dim
#' @template arg_minimize
#' @template arg_lower
#' @template arg_upper
#' @param mu [\code{integer(1)}]\cr
#'   Number of individuals in the population.
#'   Default is 100.
#' @param ref.point [\code{numeric}]\cr
#'   Reference point for the hypervolume computation. Default is (11, ..., 11)'
#'   with the corresponding dimension.
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_terminators
#' @param ... [any]\cr
#'   Further arguments passed down to fitness function.
#' @return [\code{ecr_multi_objective_result}]
#' @export
smsemoa = function(
  fitness.fun,
  n.objectives = NULL,
  n.dim = NULL,
  minimize = NULL,
  lower = NULL,
  upper = NULL,
  mu = 100L,
  ref.point = NULL,
  mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
  recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
  terminators = list(stopOnIters(100L)),
  ...) {

  if (is.null(ref.point)) {
    if (is.null(n.objectives)) {
      stopf("[smsemoa] Reference point default can only be generated if n.objectives is passed.")
    }
    ref.point = rep(11, n.objectives)
  }
  assertNumeric(ref.point, len = n.objectives)

  res = ecr(fitness.fun = fitness.fun, n.objectives = n.objectives,
    n.dim = n.dim, minimize = minimize, lower = lower, upper = upper,
    mu = mu, lambda = 1L, representation = "float", survival.strategy = "plus",
    parent.selector = selSimple,
    mutator = mutator,
    recombinator = recombinator,
    survival.selector = setup(selDomHV, ref.point = ref.point),
    terminators = terminators)

  return(res)
}
