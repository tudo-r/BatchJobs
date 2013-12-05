library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(BatchJobs)  
}

source("inst/tests/helpers.R")
test_dir("inst/tests")

