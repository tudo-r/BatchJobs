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
  
  conf$cluster.functions = makeClusterFunctionsTorque("/home/bischl/lido.tmpl")
  doExternalTest(whitespace=FALSE, sleep.master=15)
  doKillTest()

  # spam queue
  # FIXME: better test?
  conf$cluster.functions = makeClusterFunctionsTorque("/home/bischl/lido.tmpl")
  doExternalTest(whitespace=FALSE, n=50, resources=list(walltime=3*24*60*60),
    sleep.master=100, sleep.job=10)
}

runTests(staged.queries=FALSE)
runTests(staged.queries=TRUE)