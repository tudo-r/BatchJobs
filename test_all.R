library(methods)
library(devtools)
library(testthat)
library(BBmisc)
library(fail)
options(BBmisc.ProgressBar.style = "off")

if (interactive()) {
  library(RSQLite)
  library(digest)
  load_all(".")
  assignConfDefaults()
  cf = makeClusterFunctionsUnitTests()
  conf = getBatchJobsConf()
  conf$interactive = TRUE
} else {
  library("BatchJobs")
  BatchJobs:::assignConfDefaults()
  cf = BatchJobs:::makeClusterFunctionsUnitTests()
  conf = BatchJobs:::getBatchJobsConf()
}

conf$cluster.functions = cf

source("inst/tests/helpers.R")

test_dir("inst/tests")

# remove bmq stuff
lapply(list.files("inst/tests/", pattern="bmq_", full.names=TRUE), unlink, recursive=TRUE)
