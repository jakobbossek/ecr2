context("normalization of Pareto front")

test_that("normalizeFront works well", {
  pf = matrix(runif(20L, min = 5, max = 50), ncol = 10L)

  # now normalize
  pfn = normalizeFront(pf)
  # in each dimension there is one zero point
  expect_equal(sum(pfn == 0), 2L)
  # all point should be between 0 and 1
  expect_true(all(pfn >= 0 & pfn <= 1))
})
