context("termination codes")

test_that("stopping conditions work", {
  runEA = function(terminators) {
    ecr(ONEMIN, representation = "binary", n.objectives = 1L, n.bits = 5L,
      terminators = terminators,
      mu = 20L, lambda = 10L, survival.strategy = "plus")
  }

	# check for max iterations
	expect_true(grepl("iterations", runEA(list(stopOnIters(2L)))$message))

  # check for max evaluations
  expect_true(grepl("evaluations", runEA(list(stopOnEvals(30L)))$message))

  # check closeness to optimum
  opt.y = 0
  eps = 1
  expect_true(grepl("optimum", runEA(list(stopOnOptY(opt.y = opt.y, eps = eps)))$message))
})
