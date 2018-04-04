context("plotFront")

# test_that("plotFront works as expected", {
#   df = mtcars[, c("mpg", "hp")]

#   pl = plotFront(df)
#   # check for class
#   expect_class(pl, "ggplot")
#   # check for labels
#   expect_string(pl$labels$x, pattern = "min")
#   expect_string(pl$labels$y, pattern = "min")
#   expect_data_frame(pl$data, ncols = 2L, nrows = 8L) # there should be 8 nondominated

#   # now we want to maximize horsepower
#   pl = plotFront(df, minimize = c(TRUE, FALSE))
#   # check for class
#   expect_class(pl, "ggplot")
#   # check for labels
#   expect_string(pl$labels$x, pattern = "min")
#   expect_string(pl$labels$y, pattern = "max")
#   expect_data_frame(pl$data, ncols = 2L, nrows = 3L) # there should be 3 nondominated

#   # check fitness matrix plotter
#   df = t(as.matrix(df))
#   mat = makeFitnessMatrix(df, list(task = list(minimize = c(TRUE, FALSE))))

#   pl = plotFront(mat)
#   # check for class
#   expect_class(pl, "ggplot")
# })

test_that("plotScatter{2,3}d works", {
  data(mcMST)
  testdata = subset(mcMST, prob %in% paste0("prob", 1:3))

  # test 2d scatterplots
  expect_error(plotScatter2d(testdata, obj.cols = c("non", "sense")))

  testdata.malforemed = testdata
  testdata.malforemed$f1 = as.character(testdata.malforemed$f1)
  expect_error(plotScatter2d(testdata.malforemed), pattern = "not numeric")

  pl = plotScatter2d(mcMST, shape = "algorithm", colour = "algorithm", title = "test", subtitle = "to test plots")
  expect_class(pl, "ggplot")

  # should work if only objective columns are passed
  pl = plotScatter2d(mcMST[, 1:2])
  expect_class(pl, "ggplot")

  # now 3d scatterplots
  # (here we test scatterplot3d and plot3D only and not the HTML/RGL stuff)
  testdata$f3 = testdata$f1

  #FIXME: how to test this stuff?
  # for (package in c("scatterplot3d", "plot3D")) {
  #   expect_output(plotScatter3d(testdata, max.in.col = 2L, package = package))
  # }
})
