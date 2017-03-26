context("dominates and isDominated")

test_that("dominates, isDominated, isMaximallyDominated works as expected", {
  expect_true(dominates(c(1, 2), c(2, 3)))
  expect_true(isDominated(c(2, 3), c(1, 2)))
  expect_true(dominates(c(1, 2), c(1, 3)))
  expect_false(dominates(c(1, 2), c(1, 2)))
  expect_false(dominates(c(1, 2), c(2, 1)))
  expect_equal(dominates(c(1, 2), c(2, 2)), c(1, 2) %dominates% c(2, 2))
  expect_equal(dominates(c(3, 2), c(2, 2)), c(3, 2) %dominates% c(2, 2))
  expect_equal(isDominated(c(1, 2), c(2, 2)), c(1, 2) %isDominatedBy% c(2, 2))
  expect_equal(isDominated(c(3, 2), c(2, 2)), c(3, 2) %isDominatedBy% c(2, 2))
  expect_false(isDominated(c(1, 2), c(2, 1)))
  expect_false(dominates(c(843.3, 2313.3), c(42.2, 654.3324)))
  expect_equal(isMaximallyDominated(matrix(c(2, 1, 1, 2, 5, 5), ncol = 3L)), c(FALSE, FALSE, TRUE))
})

test_that("[which.{non}]dominated works well on matrices", {
  m = t(matrix(
    c(1, 1, 1, 1,
      2, 2, 3, 1,
      2, 1, 2, 1,
      2, 6, 1, 1),
    byrow = TRUE, ncol = 4L
  ))
  dom = dominated(m)
  nondom = nondominated(m)
  # check that there is boolean value for each point
  expect_equal(length(dom), ncol(m))

  # check that we really have three dominated points
  expect_equal(sum(dom), 3L)

  # ... and one non-dominated point (last column)
  expect_equal(sum(nondom), 1L)

  # get indizes of dominated and nondominated points
  dom.idxs = which.dominated(m)
  nondom.idxs = which.nondominated(m)

  expect_true(setequal(dom.idxs, 2:4))
  expect_true(nondom.idxs == 1L)
  expect_true(setequal(c(dom.idxs, nondom.idxs), seq(ncol(m))))
})
