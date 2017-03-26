context("evaluateFitness helper")

test_that("evaluation works for single-objective functions", {
  funs = list(
    smoof::makeSphereFunction(5L),
    smoof::makeAckleyFunction(5L),
    smoof::makeZDT1Function(5L)
  )
  for (fitness.fun in funs) {
    control = initECRControl(fitness.fun)
    population = replicate(5L, runif(5L), simplify = FALSE)
    fitness = evaluateFitness(control, population)
    expect_matrix(fitness, mode = "numeric", nrows = getNumberOfObjectives(fitness.fun),
      ncols = getNumberOfParameters(fitness.fun),
      any.missing = FALSE, all.missing = FALSE)
  }
})

test_that("evaluation works for single-objective vectorized functions", {
  fitness.fun = smoof::makeBBOBFunction(iid = 1L, fid = 1L, dimension = 5L)
  control = initECRControl(fitness.fun)
  population = replicate(5L, runif(5L), simplify = FALSE)
  fitness = evaluateFitness(control, population)
  expect_matrix(fitness, mode = "numeric", nrows = 1L, ncols = 5L,
    any.missing = FALSE, all.missing = FALSE)
})
