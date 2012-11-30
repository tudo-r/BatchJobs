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
  makeSSHWorker("rao"))
doExternalTest(whitespace=FALSE)
doKillTest()

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("rao", ncpus=4, max.load=4, max.jobs=2))
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsSSH(
  makeSSHWorker("rao"), 
  makeSSHWorker("compute2"))
doExternalTest(whitespace=FALSE)
doKillTest()

nodes = c("compute2", "rao")

info = getSSHWorkersInfo(nodes)
expect_true(is.list(info) && length(info) == 2)

y = callFunctionOnSSHWorkers(nodes, sqrt, 9, use.names=FALSE)
expect_equal(y, rep(3, length(nodes)))

installPackagesOnSSHWorkers(nodes, "BBmisc")
