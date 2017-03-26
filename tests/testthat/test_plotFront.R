context("plotFront")

test_that("plotFront works as expected", {
  df = mtcars[, c("mpg", "hp")]

  pl = plotFront(df)
  # check for class
  expect_class(pl, "ggplot")
  # check for labels
  expect_string(pl$labels$x, pattern = "min")
  expect_string(pl$labels$y, pattern = "min")
  expect_data_frame(pl$data, ncols = 2L, nrows = 8L) # there should be 8 nondominated

  # now we want to maximize horsepower
  pl = plotFront(df, minimize = c(TRUE, FALSE))
  # check for class
  expect_class(pl, "ggplot")
  # check for labels
  expect_string(pl$labels$x, pattern = "min")
  expect_string(pl$labels$y, pattern = "max")
  expect_data_frame(pl$data, ncols = 2L, nrows = 3L) # there should be 3 nondominated

  # check fitness matrix plotter
  df = t(as.matrix(df))
  mat = makeFitnessMatrix(df, list(task = list(minimize = c(TRUE, FALSE))))

  pl = plotFront(mat)
  # check for class
  expect_class(pl, "ggplot")
})
