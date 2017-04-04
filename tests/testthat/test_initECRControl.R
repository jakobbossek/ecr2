context("generation of control object works fine")

test_that("general control object generator", {
  control = initECRControl(function(x) sum(x), minimize = FALSE, n.objectives = 5L)
  expect_class(control, "ecr_control")
  expect_class(control$task, "ecr_optimization_task")
})
