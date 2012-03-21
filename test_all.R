library("methods")
library("devtools")
library("testthat")

if (interactive()) {
  load_all("skel")
  cf <- makeClusterFunctionsUnitTests()
} else {
  library("BatchJobs")
  cf <- BatchJobs:::makeClusterFunctionsUnitTests()
}

setBatchJobsConf(cluster.functions=cf,
                 mail.start="none",
                 mail.done="none",
                 mail.error="none",
                 mail.from="bernd_bischl@gmx.net",
                 mail.to="bernd_bischl@gmx.net",
                 mail.control=list(smtpServer="mail"))

source("skel/inst/tests/helpers.R")

if (interactive())
  assign("interactive", TRUE, envir=getBatchJobsConf())

test_dir("skel/inst/tests")
print(ls(all=TRUE))
