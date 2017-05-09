#' @title
#' Implementation of the NSGA-II EMOA algorithm by Deb.
#'
#' @description
#' The AS-EMOA, short for aspiration set evolutionary multi-objective
#' algorithm aims to incorporate expert knowledge into multi-objective optimization [1].
#' The algorithm expects an aspiration set, i.e., a set of reference points. It
#' then creates an approximation of the pareto front close to the aspiration set
#' utilizing the average Hausdorff distance.
#'
#' @note
#' This is a pure R implementation of the AS-EMOA algorithm. It hides the regular
#' ecr interface and offers a more R like interface while still being quite
#' adaptable.
#'
#' @keywords optimize
#'
#' @references
#' [1] Rudolph, G., Schuetze, S., Grimme, C., Trautmann, H: An Aspiration Set
#' EMOA Based on Averaged Hausdorff Distances. LION 2014: 153-156.
#' [2] G. Rudolph, O. Schuetze, C. Grimme, and H. Trautmann: A Multiobjective
#' Evolutionary Algorithm Guided by Averaged Hausdorff Distance to Aspiration
#' Sets, pp. 261-273 in A.-A. Tantar et al. (eds.): Proceedings of EVOLVE - A
#' bridge between Probability, Set Oriented Numerics and Evolutionary Computation
#' V, Springer: Berlin Heidelberg 2014.
#'
#' @template arg_fitness_fun
#' @template arg_n_objectives
#' @template arg_minimize
#' @template arg_n_dim
#' @template arg_lower
#' @template arg_upper
#' @param mu [\code{integer(1)}]\cr
#'   Population size. Default is 10.
#' @param aspiration.set [\code{matrix}]\cr
#'   The aspiration set. Each column contains one point of the set.
#' @param normalize.fun [\code{function}]\cr
#'   Function used to normalize fitness values of the individuals
#'   before computation of the average Hausdorff distance.
#'   The function must have the formal arguments \dQuote{set} and \dQuote{aspiration.set}.
#'   Default is \code{NULL}, i.e., no normalization at all.
#' @param dist.fun [\code{function}]\cr
#'   Distance function used internally by Hausdorff metric to compute distance
#'   between two points. Expects a single vector of coordinate-wise differences
#'   between points.
#'   Default is \code{computeEuclideanDistance}.
#' @param p [\code{numeric(1)}]\cr
#'   Parameter \eqn{p} for the average Hausdorff metric. Default is 1.
#' @template arg_parent_selector
#' @template arg_mutator
#' @template arg_recombinator
#' @template arg_terminators
# @param ... [any]\cr
#   Further arguments passed to \code{\link{setupECRControl}}.
#' @return [\code{ecr_multi_objective_result}]
# @example examples/ex_asemoa.R
#' @export
asemoa = function(
  fitness.fun,
  n.objectives = NULL,
  minimize = NULL,
  n.dim = NULL,
  lower = NULL,
  upper = NULL,
  mu = 10L,
  aspiration.set = NULL,
  normalize.fun = NULL,
  dist.fun = ecr:::computeEuclideanDistance,
  p = 1,
  parent.selector = setup(selSimple),
  mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
  recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
  terminators = list(stopOnIters(100L))) {

  if (isSmoofFunction(fitness.fun)) {
    n.dim = getNumberOfParameters(fitness.fun)
    par.set = getParamSet(fitness.fun)
    lower = getLower(par.set)
    upper = getUpper(par.set)
  }
  assertMatrix(aspiration.set, mode = "numeric", any.missing = FALSE,
    all.missing = FALSE, min.rows = 2L)
  # if (nrow(aspiration.set) != task$n.objectives) {
  #   stopf("AS-EMAO: Dimension of the aspiration set needs to be equal to the number of objectives,
  #     but %i <> %i.", nrow(aspiration.set), task$n.objectives)
  # }
  n.archive = ncol(aspiration.set)

  assertInt(mu, lower = 5L)
  assertInt(n.archive, lower = 3L)
  if (!is.null(normalize.fun)) {
    assertFunction(normalize.fun, args = c("set", "aspiration.set"), ordered = TRUE)
  }
  assertFunction(dist.fun)
  assertNumber(p, lower = 0.001)

  force(aspiration.set)
  force(n.dim)
  force(lower)
  force(upper)

  # This is the main selection mechanism of the AS-EMOA.
  # Remove the point which leads to highest
  deltaOneUpdate = function(set, aspiration.set) {
    if (!is.null(normalize.fun)) {
      set = normalize.fun(set, aspiration.set)
    }
    return(computeAverageHausdorffDistance(set, aspiration.set, p = p, dist.fun = dist.fun))
  }

  fastASEMOASelector = makeSelector(
    selector = function(fitness, n.select, par.list) {
      n.archive = ncol(aspiration.set)

      # get nondominated points
      nondom.idx = which.nondominated(fitness)
      fitness.pop = fitness[, nondom.idx, drop = FALSE]
      if (!is.null(normalize.fun)) {
        fitness.pop = normalize.fun(fitness.pop, aspiration.set)
      }
      n.pop = length(nondom.idx)

      # archive size exceeded! We need to drop one individual from the population
      if (n.pop <= n.archive) {
        return(nondom.idx)
      }

      GDp = 0
      IGDp = 0

      GDps = numeric(n.pop)
      IGDps = numeric(n.pop)

      # initialize for later use
      IGDp1s = rep(0.0, n.pop)
      IGDp2s = rep(0.0, n.pop)

      for (i in seq_len(n.pop)) {
        # GP_p contribution of archive point a
        GDps[i] = computeDistanceFromPointToSetOfPoints(fitness.pop[, i], aspiration.set)
        # add GD_p contribution of a
        GDp = GDp + GDps[i]
      }

      for (i in seq_len(n.archive)) {
        r = aspiration.set[, i]
        # if (!is.null(normalize.fun)) {
        #   fitness.pop2 = normalize.fun(fitness.pop, aspiration.set)
        # }
        rdists = computeDistancesFromPointToSetOfPoints(r, fitness.pop)
        astar.idx = which.min(rdists)
        d1 = rdists[astar.idx] # distance to closest population point
        d2 = min(rdists[-astar.idx]) # distance to 2nd closest population point
        IGDp = IGDp + d1 # add IGD_p contribution of r
        IGDp1s[astar.idx] = IGDp1s[astar.idx] + d1 # sum IGD_p contributions with a* involved
        IGDp2s[astar.idx] = IGDp2s[astar.idx] + d2 # sum IGD_p contributions without a*
      }
      dpmin = Inf
      gdpmin = Inf
      for (i in seq_len(n.pop)) {
        gdp = GDp - GDps[i] # value of GD_p if a deleted
        igdp = IGDp - IGDp1s[i] + IGDp2s[i] # value of IGD_p if a deleted
        dp = max(gdp / (n.pop - 1), igdp / n.archive) # delta_1 if a deleted
        if (dp < dpmin | (dp == dpmin & gdp < gdpmin)) {
          dpmin = dp # store smallest delta_1 seen so far
          gdpmin = gdp # store smallest gdp since last improvement
          astar.idx = i
        }
      }
      return(nondom.idx[-astar.idx])
    },
    supported.objectives = "multi-objective"
  )

  # Implementation of surival selection operator of the AS-EMOA algorithm.
  asemoaSelector = makeSelector(
    selector = function(fitness, n.select) {
      n.archive = ncol(aspiration.set)

      # get offspring
      all.idx = 1:ncol(fitness)

      # filter nondominated points
      nondom.idx = which.nondominated(fitness)
      pop.idx = all.idx[nondom.idx]
      fitness = fitness[, nondom.idx, drop = FALSE]

      # if maximal number of individuals is not exceeded yet
      # simply return
      if (length(pop.idx) <= n.archive) {
        return(pop.idx)
      }

      # Otherwise we need to do the computationally more expensive part
      has = lapply(pop.idx, function(idx) {
        deltaOneUpdate(fitness[, -idx, drop = FALSE], aspiration.set)
      })

      # set of elements whose deletion from archive leads to
      # highest decrese of delta_p
      astar = which.min(has)

      # if there are multiple points, choose the one which leads
      # to minimal generational distance if removed
      if (length(astar) > 1L) {
        generationalDistances = lapply(astar, function(idx) {
          computeGenerationalDistance(fitness[, -idx, drop = FALSE], aspiration.set, p = p, dist.fun = dist.fun)
        })
        # determine which is minimal
        min.idx = getMinIndex(generationalDistances)
        astar = astar[min.idx]
      } else {
        astar = getMinIndex(has)
      }

      return(setdiff(pop.idx, astar))
    },
    supported.objectives = "multi-objective"
  )

  initial.solutions = genReal(mu, n.dim, lower, upper)
  fitness.ip = do.call(cbind, lapply(initial.solutions, fitness.fun))
  nondom.idx = which.nondominated(fitness.ip)
  initial.solutions = initial.solutions[nondom.idx]

  # AS-EMOA control object
  res = ecr(fitness.fun = fitness.fun,
    n.objectives = n.objectives, minimize = minimize,
    n.dim = n.dim, lower = lower, upper = upper,
    representation = "float",
    mu = mu, lambda = 1L,
    initial.solutions = initial.solutions,
    terminators = terminators,
    mutator = mutator,
    recombinator = recombinator,
    survival.selector = fastASEMOASelector)

  #  res = BBmisc::addClasses(res, "ecr_asemoa_result")
  return(res)
}

# @title
# Normalization function introduced in [1].
#
# @param set [\code{matrix}]\cr
#   Fitness values of the candidate solutions.
# @param aspiration.set [\code{matrix}]\cr
#   Aspiration set.
# @return [\code{matrix}]
asemoaNormalize1 = function(set, aspiration.set) {
  min1 = min(aspiration.set[1L, ])
  min2 = min(aspiration.set[2L, ])
  max1 = max(aspiration.set[1L, ])
  max2 = max(aspiration.set[2L, ])

  # transform
  set[1L, ] = (set[1L, ] - min1) / (max2 - min2)
  set[2L, ] = (set[2L, ] - min2) / (max1 - min1)
  return(set)
}
