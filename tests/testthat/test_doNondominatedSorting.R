context("nondominated sorting")

test_that("ranking nondominated fronts works as expected", {
  # test set of points with 4 domination layerrs
  n.fronts = 4L
  x = t(matrix(
    c(# front 1
      1, 4,
      2, 3,
      4, 1,
      # front 2
      2.2, 3.2,
      4, 3,
      4.2, 1,
      # front 3
      3, 5,
      3.2, 4.7,
      6, 2,
      # front 4
      6, 6), byrow = TRUE, ncol = 2L))

  res = doNondominatedSorting(x)

  # check structure of the result object
  expect_true(is.list(res))
  expect_equal(length(res), 2L)
  expect_true(is.integer(res$ranks))
  expect_true(is.integer(res$dom.counter))

  # number of ranks equals expected number of fronts
  expect_equal(length(unique(res$ranks)), n.fronts)

  # each point can be dominated by at most n-1 points
  expect_true(all(res$dom.counter < ncol(x)))

  # check if fronts are computed correctly
  tt = as.numeric(table(res$ranks))
  expect_true(all(tt == c(3L, 3L, 3L, 1L)))
})
