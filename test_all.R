library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(ecr)
}

test_dir("tests/testthat")
