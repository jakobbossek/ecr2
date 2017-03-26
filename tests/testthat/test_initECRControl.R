context("generation of control object works fine")

test_that("general control object generator", {
  control = initECRControl(function(x) sum(x), minimize = FALSE, n.objectives = 5L)
  expect_class(control, "ecr2_control")
  expect_class(control$task, "ecr2_optimization_task")
})
