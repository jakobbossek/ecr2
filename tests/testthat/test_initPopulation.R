context("build initial population")

expect_population = function(population, mu, len) {
  # check population type
  expect_true(is.list(population))
  # check population length
  expect_equal(length(population), mu)
  # check length of each individual
  expect_true(all(sapply(population, length) == len))
}

expect_binary_population = function(population, mu, len) {
  expect_population(population, mu, len)
  # check binary'ness
  expect_true(all(sapply(population, function(ind) all(ind %in% c(0, 1)))))
}

test_that("binary generator works well", {
  len = 10L
  mu = 5L
  generateFun = setupBinaryGenerator(len)
  population = generateFun(mu)
  expect_binary_population(population, mu, len)
})

test_that("uniform generator works well", {
  len = 10L
  mu = 5L
  lower = -10; upper = 3
  generateFun = setupUniformGenerator(len, lower = lower, upper = upper)
  population = generateFun(mu)
  expect_population(population, mu, len)
  # check that all individuals are numeric vectors
  expect_true(all(sapply(population, is.numeric)))
  # check that all elements are in [lower, upper]
  expect_true(all(sapply(population, function(ind) {
    all(ind >= lower & ind <= upper)
  })))
})

test_that("permutation generator works well", {
  len = 10L
  mu = 5L
  generateFun = setupPermutationGenerator(len)
  population = generateFun(mu)
  expect_population(population, mu, len)
  # check all elements are in fact permutation of 1:len
  expect_true(all(sapply(population, function(ind) {
    setequal(ind, 1:len)
  })))
})

test_that("initPopulation helper works well", {
  len = 10L
  mu = 10L
  generateFun = setupBinaryGenerator(len)
  control = initECRControl()
  control = registerGenerator(control, generateFun)
  # now first generate without initial solutions
  population = initPopulation(mu, control = control)
  expect_binary_population(population, mu, len)
  # now with initial solutions
  # generate some initial solutions
  init.solutions = generateFun(mu / 2)
  population = initPopulation(mu, control = control, init.solutions = init.solutions)
  expect_binary_population(population, mu, len)
  # check that first mu/2 solutions of te generated population are indeed
  # the initial solutions passed
  for (i in 1:(mu / 2)) {
    expect_equal(population[[i]], init.solutions[[i]])
  }
})
