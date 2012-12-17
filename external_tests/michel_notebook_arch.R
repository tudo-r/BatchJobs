library(BatchJobs)
source("helpers.R")

runTests = function(staged.queries) {
  conf = BatchJobs:::getBatchJobsConf()
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  conf$staged.queries = staged.queries
  
  conf$cluster.functions = makeClusterFunctionsInteractive()
  doExternalTest(whitespace=FALSE)
  
  conf$cluster.functions = makeClusterFunctionsLocal()
  doExternalTest(whitespace=FALSE)
  
  conf$cluster.functions = makeClusterFunctionsMulticore()
  doExternalTest(whitespace=FALSE, n=2)
  doKillTest(n=2)
}

runTests(staged.queries=FALSE)
runTests(staged.queries=TRUE)