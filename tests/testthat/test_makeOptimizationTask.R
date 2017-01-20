context("generation of optimization tasks works well")

test_that("optimization tasks are properly generated", {
  # first pass a smoof function and check if all parameters are set appropriately
  dims = c(2L, 3L, 5L)
  for (dim in dims) {
    fn = smoof::makeZDT1Function(dim)
    task = makeOptimizationTask(fn)
    # by default all objectives should be minimized
    expect_true(all(task$minimize))
    expect_equal(length(task$minimize), 2L)
    expect_equal(task$n.objectives, 2L)
    expect_output(print(task), "optimization task", ignore.case = TRUE)
  }

  # now generate a task by hand
  fn = makeZDT2Function(2L)
  task = makeOptimizationTask(fn, minimize = c(TRUE, FALSE), objective.names = c("warmth", "elasticity"))
  expect_true(any(task$minimize))
  expect_true(any(!task$minimize))
  expect_true(all(task$objective.names == c("warmth", "elasticity")))
  expect_equal(task$n.objectives, 2L)

  # now check for errors
  fn = makeSphereFunction(2L)
  # wrong number of objectives
  expect_error(makeOptimizationTask(fn, n.objectives = 2L))
  # wrong length of minimize parameter
  expect_error(makeOptimizationTask(fn, minimize = c(TRUE, FALSE, TRUE)))

  # check if warning is printed if function with requires/forbidden is passed
  fn = makeSingleObjectiveFunction(
    "FUN",
    fn = function(x) x,
    par.set = makeParamSet(makeNumericParam("x", requires = expression(x^2 > 0)))
  )
  expect_warning(makeOptimizationTask(fn), "require", ignore.case = TRUE)
})
