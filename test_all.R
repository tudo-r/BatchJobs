library("methods")
library("BBmisc")
library("devtools")
library("testthat")
options(BBmisc.ProgressBar.style = "off")

if (interactive()) {
  load_all("skel")
  library(RSQLite)
  library(digest)
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

source("skel/inst/tests/helpers.R")

test_dir("skel/inst/tests")
print(ls(all=TRUE))

# remove bmq stuff
lapply(list.files("skel/inst/tests/", pattern="bmq_", full.names=TRUE), unlink, recursive=TRUE)
