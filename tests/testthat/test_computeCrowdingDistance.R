context("crowding distance")

test_that("crowding distance computation works well", {
  # set of nondominated points
  x = t(matrix(c(
    1, 4, # cd: oo
    2, 3, # cd: 3
    2.5, 2.5, # cd: 3
    4, 2 # cd: oo
  ), byrow = TRUE, ncol = 2L))

  # compute crowding distances
  fns = c(computeCrowdingDistance, computeCrowdingDistanceR)
  for (fn in fns) {
    cds = fn(x)

    expect_true(is.numeric(cds))

    # the "extreme" points have infinite crowding distance
    expect_true(is.infinite(cds[1]))
    expect_true(is.infinite(cds[4]))

    # the remaining points have a crowding distance of 3
    expect_equal(cds[2], 3)
    expect_equal(cds[3], 3)
  }

})
