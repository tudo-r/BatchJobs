library(BatchJobs)
source("helpers.R")

conf = BatchJobs:::getBatchJobsConf()
conf$mail.start = conf$mail.done = conf$mail.error = "none"

conf$cluster.functions = makeClusterFunctionsInteractive()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsLocal()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsMulticore()
doExternalTest(whitespace=FALSE)
doKillTest(reg)

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("rao"))
doExternalTest(whitespace=FALSE)
doKillTest()

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("rao", ncpus=4, max.load=4, max.jobs=2))
doExternalTest(whitespace=FALSE)
doKillTest()

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("rao"), 
  makeSSHWorker("compute1"))
doExternalTest(whitespace=FALSE)
doKillTest()



