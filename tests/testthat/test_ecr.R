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
        expect_class(res, "ecr2_result")
        expect_equal(res$best.y, 0)
        expect_true(all(unlist(res$best.x) == 0))
      }
    }
  }
})

test_that("real-valued smoof function optimization works", {
  fitness.fun = smoof::makeSphereFunction(dimensions = 2L)
  max.gens = 50L
  for (mu in c(15, 30)) {
    for (lambda in c(15, 30)) {
      for (survival.strategy in c("plus", "comma")) {
        n.elite = if (survival.strategy == "comma") floor(mu / 3) else 0
        res = ecr(fitness.fun = fitness.fun, n.dim = getNumberOfParameters(fitness.fun),
          n.objectives = 1L, minimize = TRUE,
          mu = mu, lambda = lambda, n.elite = n.elite,
          terminators = list(stopOnIters(max.gens)),
          representation = "float")
        expect_class(res, "ecr2_result")
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
