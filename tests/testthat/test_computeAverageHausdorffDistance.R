context("average Hausdorff distance/metric computation")

test_that("computation of average Hausdorff distance yields reasonable results", {
  # first we generate two point clouds with large distance
  n.points = 20L
  pc1 = matrix(runif(n.points * 2L), nrow = 2L)
  pc2 = matrix(runif(n.points * 2L), nrow = 2L)

  # Now delete one row and expect error
  expect_error(computeAverageHausdorffDistance(pc1, pc2[-1L, , drop = FALSE]))

  # Now compute the Hausdorff distance repeatedly and increase the distance of the clouds
  # The sequence should be increasing.
  factors = 2:10
  vals = numeric(length(factors) + 1L)
  vals[1L] = computeAverageHausdorffDistance(pc1, pc2)
  i = 2L
  while (i <= length(factors)) {
    pc = pc2 * factors[i]
    vals[i] = computeAverageHausdorffDistance(pc1, pc)
    expect_true(is.numeric(vals[i]), info = sprintf("Hausdorff distance not numeric for factor %i.", factors[i]))
    expect_true(vals[i-1] < vals[i], info = sprintf("Hausdorff distance should increase for increasing
      distance of the point clouds for factor %i.", factors[i]))
    i = i + 1L
  }
})
