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
  hv = computeHV(points, ref.point)
  expect_true(is.numeric(hv))
  expect_equal(hv, hv.exp)

  # HV with self computed reference point
  hv = computeHV(points)
  expect_true(is.numeric(hv))

  # check sanity checks
  # Unequal dimensions
  expect_error(computeHV(points, ref.point[-1L]))

  # check for warnings on infinite values
  points2 = points
  points2[1L, 1L] = Inf
  expect_warning(computeHV(points2, ref.point), "point", ignore.case = TRUE)
  expect_true(suppressWarnings(is.nan(computeHV(points2, ref.point))))

  ref.point2 = ref.point
  ref.point2[2L] = Inf
  expect_warning(computeHV(points, ref.point2), "Reference point", ignore.case = TRUE)
  expect_true(suppressWarnings(is.nan(computeHV(points, ref.point2))))

  # now check the hypervolume contributions
  hv.contribs = computeHVContr(points, ref.point)
  expect_true(all(hv.contribs == 1))
  # the computed reference point should be equal to (6,6) too
  hv.contribs = computeHVContr(points)
  expect_true(all(hv.contribs == 1))
})

test_that("assertions on hypervolume (contribution)", {
  n.points = 100L
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

    hv1 = computeHV(x1, ref.point)
    hv2 = computeHV(x2, ref.point)

    expect_true(hv1 >= 0)
    expect_true(hv2 >= 0)
    expect_true(hv1 > hv2, info = "HV of first non-dominanted front should be
      larger than the dominated HV of the second.")
    hvctrb1 = computeHVContr(x1, ref.point)
    hvctrb2 = computeHVContr(x2, ref.point)
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
  expect_equal(emoaIndR1(points = points, ref.points = points), 0.5)
  expect_equal(emoaIndR2(points = points, ref.points = points), 0)
  expect_equal(emoaIndR3(points = points, ref.points = points), 0)
})

test_that("HyperVolume indicator is computed correctly", {
  # here all the points in the approximation are located on a line
  #   |
  #   |
  # 0 |  o    o
  #   |   o     o
  #   |     o     o
  #   |       o
  #   |          o
  #   __________________
  #               0

  # generate random convex Pareto front shapes, like the one of DTLZ1
  randConvex = function(n, r) {
    alphas = runif(n, pi, 1.5 * pi) # draw random degrees from [pi, 3/2 pi]
    matrix(c(
      r * cos(alphas),
      r * sin(alphas)),
    byrow = TRUE, nrow = 2L)
  }

  # radius of pf and pf approx
  rpf = 3
  rpfa = 2

  # now check if HV indicator is always > 0 and moreover check if in this case
  # the HV indicator value approximates the theoretical area included between the
  # fronts
  for (i in 1:10) {
    pf = randConvex(1000, rpf)
    pfa = randConvex(1000, rpfa)
    r = c(0, 0)
    hvi = emoaIndHV(pfa, pf, r)
    theoretical = pi * (rpf^2 - rpfa^2) / 4
    expect_true(hvi >= 0)
    expect_true(abs(hvi - theoretical) < 0.01)
    expect_true(emoaIndEps(pfa, pf) > 0)
  }
})
