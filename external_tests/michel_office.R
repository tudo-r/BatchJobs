library(BatchJobs)
source("helpers.R")

runTests = function(staged.queries) {
  conf = BatchJobs:::getBatchJobsConf()
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  conf$staged.queries = staged.queries

  conf$cluster.functions = makeClusterFunctionsInteractive()
  doExternalTest(dir = "/home/lang/store", whitespace=FALSE)

  conf$cluster.functions = makeClusterFunctionsLocal()
  doExternalTest(dir = "/home/lang/store", whitespace=FALSE)

  conf$cluster.functions = makeClusterFunctionsMulticore()
  doExternalTest(dir = "/home/lang/store", whitespace=FALSE)
  doKillTest(dir = "/home/lang/store")

  conf$cluster.functions = makeClusterFunctionsSSH(makeSSHWorker("lang", nice=9))
  doExternalTest(dir = "/home/lang/store", whitespace=FALSE)
  doKillTest(dir = "/home/lang/store")

  conf$cluster.functions = makeClusterFunctionsSSH(makeSSHWorker("lang", ncpus=4, max.load=4, max.jobs=2))
  doExternalTest(dir = "/home/lang/store", whitespace=FALSE)
  conf$cluster.functions = makeClusterFunctionsSSH(makeSSHWorker("lang", ncpus=4, max.load=4, max.jobs=4))
  doKillTest(dir = "/home/lang/store")

  conf$cluster.functions = makeClusterFunctionsSSH(makeSSHWorker("lang", ncpus=2), makeSSHWorker("rao", rhome="/opt/R/R-current"))
  reg = doExternalTest(dir = "/home/lang/store", whitespace=FALSE)
  doKillTest(dir = "/home/lang/store/")
}

runTests(staged.queries=FALSE)
runTests(staged.queries=TRUE)
