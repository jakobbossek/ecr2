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
  n.bits = 10L
  mu = 5L
  population = genBin(mu, n.bits)
  expect_binary_population(population, mu, n.bits)
})

test_that("uniform generator works well", {
  len = 10L
  mu = 5L
  lower = -10; upper = 3
  population = genReal(mu, len, lower, upper)
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
  population = genPerm(mu, len)
  expect_population(population, mu, len)
  # check all elements are in fact permutation of 1:len
  expect_true(all(sapply(population, function(ind) {
    setequal(ind, 1:len)
  })))
})

test_that("initPopulation helper works well", {
  n.bits = 10L
  mu = 10L
  # now first generate without initial solutions
  population = initPopulation(mu, gen.fun = genBin, n.dim = n.bits)
  expect_binary_population(population, mu, n.bits)
  # now with initial solutions
  # generate some initial solutions
  initial.solutions = genBin(mu / 2, n.dim = n.bits)
  population = initPopulation(mu, gen.fun = genBin, initial.solutions = initial.solutions, n.dim = n.bits)
  expect_binary_population(population, mu, n.bits)
  # check that first mu/2 solutions of te generated population are indeed
  # the initial solutions passed
  for (i in 1:(mu / 2)) {
    expect_equal(population[[i]], initial.solutions[[i]])
  }
})

test_that("initPopulation fails if size exceeds mu", {
  expect_error(initPopulation(5L, gen.fun = genBin, initial.solutions = genBin(10L, 10L), n.bits = 10L))
})
