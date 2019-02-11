context("main black-box function ecr")

test_that("ONE-MIN optimization works", {
  n.bits = 10L
  max.gens = 100L
  for (mu in c(10, 20)) {
    for (lambda in c(10, 20)) {
      for (survival.strategy in c("plus", "comma")) {
        n.elite = if (survival.strategy == "comma") floor(mu / 3) else 0
        res = ecr(fitness.fun = ONEMIN, n.bits = n.bits,
          n.objectives = 1L, minimize = TRUE,
          mu = mu, lambda = lambda, n.elite = n.elite,
          terminators = list(stopOnIters(max.gens)),
          representation = "binary")
        expect_class(res, "ecr_result")
        expect_equal(res$best.y, 0)
        expect_true(all(unlist(res$best.x) == 0))
      }
    }
  }
})

test_that("real-valued smoof function optimization works", {
  fitness.fun = smoof::makeSphereFunction(dimensions = 2L)
  max.gens = 100L
  for (mu in c(15, 30)) {
    for (lambda in c(15, 30)) {
      for (survival.strategy in c("plus", "comma")) {
        if (survival.strategy == "comma" & lambda < mu)
          next
        n.elite = if (survival.strategy == "comma") floor(mu / 3) else 0
        res = ecr(fitness.fun = fitness.fun, n.dim = getNumberOfParameters(fitness.fun),
          n.objectives = 1L, minimize = TRUE,
          survival.strategy = survival.strategy,
          mu = mu, lambda = lambda, n.elite = n.elite,
          terminators = list(stopOnIters(max.gens)),
          mutator = ecr::setup(mutGauss, lower = getLowerBoxConstraints(fitness.fun),
            upper = getUpperBoxConstraints(fitness.fun)),
          representation = "float")
        expect_class(res, "ecr_result")
        expect_false(is.null(res))
        expect_true(res$best.y < 0.1,
          info = sprintf("Did not approximate optimal value with params mu: %i, lambda: %i, strategy: %s",
            mu, lambda, survival.strategy))
        expect_true(all(unlist(res$best.x) < 0.1),
          info = sprintf("Did not approximate optimal params with params mu: %i, lambda: %i, strategy: %s",
            mu, lambda, survival.strategy))
      }
    }
  }
})

test_that("permutation based problems work well", {
  # defs
  perm.len = 5L
  max.iter = 50L

  # objective
  fitness.fun = function(x) {
    CI = 0
    for (i in seq(length(x) - 1L)) {
      CI = CI + sum(x[1L] > x[-1L])
      x = x[-1L]
    }
    return(CI)
  }

  # check it for a selection of mutators for permutations
  for (mutator in c(setup(mutSwap), setup(mutInversion), setup(mutInsertion))) {
    for (recombinator in c(setup(recPMX), setup(recOX))) {
      res = ecr(fitness.fun = fitness.fun,
        representation = "permutation", perm = perm.len,
        n.objectives = 1L, minimize = TRUE,
        mu = 5L, lambda = 5L, survival.strategy = "plus",
        mutator = mutator, recombinator = recombinator)

      # check results
      expect_false(is.null(res))
      expect_equal(res$best.y, 0,
        info = sprintf("Did not find correct sorting with mutator."))
    }
  }
})

test_that("ecr works for maximization", {
  fitness.fun = makeSingleObjectiveFunction(
    name = "maximize me",
    fn = function(x) -sum(x^2),
    par.set = makeNumericParamSet("x", len = 1L, lower = -10, upper = 10),
    minimize = FALSE # we want to maximize here
  )

  res = ecr(fitness.fun = fitness.fun,
    representation = "float",
    mu = 10L, lambda = 10L, survival.strategy = "plus",
    mutator = ecr::setup(mutGauss, lower = getLowerBoxConstraints(fitness.fun),
      upper = getUpperBoxConstraints(fitness.fun)),
    terminators = list(stopOnIters(100L)))

  expect_true(abs(res$best.y - 0) < 0.05)
})

test_that("ecr logs what we want to be logged", {
  res = ecr(fitness.fun = ONEMIN, mu = 5L, lambda = 5L, survival.strategy = "plus",
    n.bits = 10L, n.objectives = 1L, representation = "binary",
    terminators = list(stopOnIters(10L)),
    log.stats = list(fitness = list("min", "max")))
  stats = getStatistics(res$log)
  expect_true(BBmisc::isSubset(c("fitness.min", "fitness.max"), colnames(stats)))
})
