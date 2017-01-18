library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(ecr2)
}

test_dir("tests/testthat")
