context("ref points")

test_that("approximate ref points", {
  # setup
  A = data.frame(badName = c("Test", "Test2"), f1 = c(1,2), f2 = c(3,4))
  B = data.frame(prob = c("Test", "Test2"), f1 = c(1,2), f2 = c(3,4))
  
  # passing dataframe with bad structure (wrong column name)
  expect_error(approximateRefPoints(A))
  
  # with correctly structured set
  ref = approximateRefPoints(B)
  expect_list(ref, len = 2)
  
})
