context("nadir/ideal point")

test_that("approximate nadir or ideal point if missing", {
  # setup
  A = matrix(c(1,2,1,3), byrow = TRUE, ncol = 2L)
  B = matrix(c(3,2,4,5), byrow = TRUE, ncol = 2L)

  # passing nonsense (matrizes with different dimensions)
  expect_error(approximateIdealPoint(matrix(1:10, nrow = 5L), A))

  # passing a single set
  ip = approximateIdealPoint(A)
  np = approximateNadirPoint(A)
  expect_true(all(ip == c(1, 1)))
  expect_true(all(np == c(2, 3)))

  # passing two sets
  ip = approximateIdealPoint(A, B)
  np = approximateNadirPoint(A, B)
  expect_true(all(ip == c(1, 1)))
  expect_true(all(np == c(3, 5)))

  # passing two sets via a list
  ip = approximateIdealPoint(sets = list(A, B))
  np = approximateNadirPoint(sets = list(A, B))
  expect_true(all(ip == c(1, 1)))
  expect_true(all(np == c(3, 5)))
})
