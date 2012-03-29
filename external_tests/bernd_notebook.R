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

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("localhost"))
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("localhost", ncpus=4, max.load=4, max.jobs=2))
doExternalTest(whitespace=FALSE)

