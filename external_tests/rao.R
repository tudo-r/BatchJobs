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
doKillTest(long="sleep")
doKillTest(long="expensive")

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("neyman"))
doExternalTest(whitespace=FALSE)
doKillTest()

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("neyman", ncpus=4, max.load=4, max.jobs=2))
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("neyman"), 
  makeSSHWorker("kendall"))
doExternalTest(whitespace=FALSE)
doKillTest()



