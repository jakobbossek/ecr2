context("mutate helper")

test_that("mutate helper works if mutator is passed", {
  inds = replicate(5, sample(c(0, 1), 10, replace = TRUE), simplify = FALSE)
  mutator = setupBitflipMutator(p = 0.1)
  mut.inds = mutate(mutator, inds, p.mut = 0.4)
  # equal length
  expect_equal(length(inds), length(mut.inds))

  # all genomes binary
  expect_true(all(unlist(mut.inds) %in% c(0, 1)))
})

test_that("mutate helper works if control is passed", {
  # get test function
  fitness.fun = smoof::makeSphereFunction(3L)
  par.set = getParamSet(fitness.fun)

  # setup control
  control = initECRControlFloat(fitness.fun = fitness.fun)
  inds = replicate(10L, runif(10L, -5, 5), simplify = FALSE)

  # now mutate with prob 1 and pass additional arguments
  mut.inds = mutate(control, inds, p.mut = 1, lower = rep(-1, 10L), upper = rep(1, 10L))

  # ... and check whether these arguments are passed to mutator
  expect_equal(length(inds), length(mut.inds))
  mut.inds = unlist(mut.inds)
  expect_true(all(mut.inds >= -1 & mut.inds <= 1))
})
