context("generation of control object works fine")

test_that("general control object generator", {
  control = initECRControl(function(x) sum(x), minimize = FALSE, n.objectives = 5L)
  expect_class(control, "ecr2_control")
  expect_class(control$task, "ecr2_optimization_task")
})

# test_that("initECRControl* work well", {
#   n.bits = 200L
#   control = initECRControlBinary(
#     fitness.fun = function(x) sum(x),
#     n.bits = n.bits, minimize = TRUE, n.objectives = 1L)
#   expect_class(control, "ecr2_control")
#   expect_class(control$task, "ecr2_optimization_task")
#   expect_character(control$type)
#   expect_list(control$params)
#   expect_equal(control$params$n.bits, n.bits)
# })

# test_that("initECRControlFloat* work well", {
#   n.bits = 200L
#   lower = rep(-5, 10); upper = rep(2, 10)
#   n.dim = length(lower)
#   control = initECRControlFloat(
#     fitness.fun = function(x) sum(x),
#     n.dim = n.dim,
#     minimize = TRUE, n.objectives = 1L,
#     lower = lower, upper = upper)
#   expect_class(control, "ecr2_control_float")
#   expect_class(control$task, "ecr2_optimization_task")
#   expect_character(control$type)
#   expect_list(control$params)
#   expect_equal(control$params$n.dim, n.dim)
#   expect_equal(control$params$lower, lower)
#   expect_equal(control$params$upper, upper)
# })

# test_that("initECRControlFloat* works well with smoof function", {
#   zdt1.fun = smoof::makeZDT1Function(21L)
#   par.set = ParamHelpers::getParamSet(zdt1.fun)
#   lower = unname(getLower(par.set))
#   upper = unname(getUpper(par.set))
#   n.dim = smoof::getNumberOfParameters(zdt1.fun)
#   n.obj = smoof::getNumberOfObjectives(zdt1.fun)

#   control = initECRControlFloat(fitness.fun = zdt1.fun)
#   expect_class(control, "ecr2_control_float")
#   expect_class(control$task, "ecr2_optimization_task")
#   expect_character(control$type)
#   expect_list(control$params)
#   expect_equal(control$params$n.dim, n.dim)
#   expect_equal(control$params$lower, lower)
#   expect_equal(control$params$upper, upper)
#   expect_equal(control$task$n.objectives, n.obj)
# })
