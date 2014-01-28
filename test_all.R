library(devtools)
library(testthat)

if (interactive()) {
  library(BBmisc)
  library(fail)
  library(DBI)
  library(sendmailR)
  library(brew)
  library(fail)
  library(RSQLite)
  library(digest)
  library(plyr)
  load_all(".")
} else {
  library("BatchJobs")
}

test_dir("inst/tests")
