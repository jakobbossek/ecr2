context("termination codes")

test_that("stopping conditions work", {
  fitness.fun = smoof::makeSphereFunction(2L)

  runEA = function(terminators) {
    ecr(fitness.fun, representation = "float", terminators = terminators,
      mu = 20L, lambda = 10L, survival.strategy = "plus")
  }

	# check for max iterations
	expect_true(grepl("iterations", runEA(list(stopOnIters(2L)))$message))

  # check for max evaluations
  expect_true(grepl("evaluations", runEA(list(stopOnEvals(30L)))$message))

  # check closeness to optimum
  opt.y = getGlobalOptimum(fitness.fun)$value
  eps = 0.05
  expect_true(grepl("optimum", runEA(list(stopOnOptY(opt.y = opt.y, eps = eps)))$message))
})
