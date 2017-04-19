context("mutate helper")

test_that("mutate helper works if control is passed", {
  # get test function
  fitness.fun = smoof::makeSphereFunction(3L)
  par.set = getParamSet(fitness.fun)

  # setup control
  control = initECRControl(fitness.fun)
  control = registerECROperator(control, "mutate", mutGauss, lower = rep(-1, 10L),
    upper = rep(1, 10L))
  inds = genReal(10L, 10L, -1, 1)

  # now mutate with prob 1 and pass additional arguments
  mut.inds = mutate(control, inds, p.mut = 1)

  # ... and check whether these arguments are passed to mutator
  expect_equal(length(inds), length(mut.inds))
  mut.inds = unlist(mut.inds)
  expect_true(all(mut.inds >= -1 & mut.inds <= 1))
})
