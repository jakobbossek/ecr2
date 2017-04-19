context("recombinate helper")

test_that("recombinate helper works if control is passed", {
  # get test function
  fitness.fun = function(x) sum(x)
  mu = 30L
  n.bits = 20L

  # setup control
  control = initECRControl(fitness.fun = fitness.fun, n.objectives = 1L)
  control = registerECROperator(control, "selectForMating", selTournament, k = 2L)
  control = registerECROperator(control, "recombine", recCrossover)

  inds = genBin(mu, n.bits)
  fitness = matrix(sapply(inds, fitness.fun), nrow = 1L)

  # now recombinate with prob 1 and pass additional arguments
  rec.inds = recombinate(control, inds, fitness, p.recomb = 1)

  # ... and check whether these arguments are passed to mutator
  expect_length(rec.inds, mu)
  rec.inds = unlist(rec.inds)
  expect_true(all(rec.inds %in% c(0, 1)))
})
