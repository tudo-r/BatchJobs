library("methods")
library("devtools")
library("testthat")

if (interactive()) {
  load_all("skel")
  cf = makeClusterFunctionsUnitTests()
  conf = getBatchJobsConf()
  conf$interactive = TRUE
} else {
  library("BatchJobs")
  cf = BatchJobs:::makeClusterFunctionsUnitTests()
  conf = BatchJobs:::getBatchJobsConf()
  
}

conf$cluster.functions = cf
conf$mail.start = "none"
conf$mail.done = "none"
conf$mail.error = "none"

source("skel/inst/tests/helpers.R")

test_dir("skel/inst/tests")
print(ls(all=TRUE))
