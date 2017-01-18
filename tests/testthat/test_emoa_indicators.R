context("Hypervolume contribution")

test_that("calculation of dominated hypervolume works as expected", {
  # here all the points in the approximation are located on a line
  # |
  # |               o (6,6)
  # |  o  (1,5)
  # |    o (2,4)
  # |      o (3,3)
  # |        o (4,2)
  # |          o (5,1)
  # __________________
  # The correct/expected dominated hypervolume value is 15 in this test case
  hv.exp = 15
  points = matrix(
    c(1, 5,
      2, 4,
      3, 3,
      4, 2,
      5, 1),
    nrow = 2L)
  ref.point = c(6, 6)

  # HV with passed reference point
  hv = computeDominatedHypervolume(points, ref.point)
  expect_true(is.numeric(hv))
  expect_equal(hv, hv.exp)

  # HV with self computed reference point
  hv = computeDominatedHypervolume(points)
  expect_true(is.numeric(hv))

  # check sanity checks
  # Unequal dimensions
  expect_error(computeDominatedHypervolume(points, ref.point[-1L]))

  # check for warnings on infinite values
  points2 = points
  points2[1L, 1L] = Inf
  expect_warning(computeDominatedHypervolume(points2, ref.point), "point", ignore.case = TRUE)
  expect_true(suppressWarnings(is.nan(computeDominatedHypervolume(points2, ref.point))))

  ref.point2 = ref.point
  ref.point2[2L] = Inf
  expect_warning(computeDominatedHypervolume(points, ref.point2), "Reference point", ignore.case = TRUE)
  expect_true(suppressWarnings(is.nan(computeDominatedHypervolume(points, ref.point2))))

  # now check the hypervolume contributions
  hv.contribs = computeHypervolumeContribution(points, ref.point)
  expect_true(all(hv.contribs == 1))
  # the computed reference point should be equal to (6,6) too
  hv.contribs = computeHypervolumeContribution(points)
  expect_true(all(hv.contribs == 1))
})

test_that("assertions on hypervolume (contribution)", {
  n.points = 50L
  n.reps = 5L
  ref.point = c(11, 11)
  for (i in seq(n.reps)) {
    # generate set of points
    x = matrix(runif(n.points * 2L, min = 0, max = 10), nrow = 2L)

    # Here we do naive nondominated sorting (later replace that with the fast
    # non-dominated sorting algorithm)
    # first non-dominanted front
    nondom.idx = which.nondominated(x)
    x1 = x[, nondom.idx, drop = FALSE]
    x = x[, -nondom.idx, drop = FALSE]

    # second non-dominanted front
    nondom.idx = which.nondominated(x)
    x2 = x[, nondom.idx, drop = FALSE]

    hv1 = computeDominatedHypervolume(x1, ref.point)
    hv2 = computeDominatedHypervolume(x2, ref.point)

    expect_true(hv1 >= 0)
    expect_true(hv2 >= 0)
    expect_true(hv1 > hv2, info = "HV of first non-dominanted front should be
      larger than the dominated HV of the second.")
    hvctrb1 = computeHypervolumeContribution(x1, ref.point)
    hvctrb2 = computeHypervolumeContribution(x2, ref.point)
    expect_true(all(hvctrb1 >= 0))
    expect_true(all(hvctrb2 >= 0))
  }
})

test_that("R{1,2,3} indicators are computed correctly.", {
  #FIXME: this is copy and paste from the HV test
  points = matrix(
    c(1, 5,
      2, 4,
      3, 3,
      4, 2,
      5, 1),
    nrow = 2L)

  # some basic checks
  expect_equal(computeR1Indicator(points = points, ref.points = points), 0.5)
  expect_equal(computeR2Indicator(points = points, ref.points = points), 0)
  expect_equal(computeR3Indicator(points = points, ref.points = points), 0)
})
