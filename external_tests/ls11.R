library(BatchJobs)
source("helpers.R")

conf = BatchJobs:::getBatchJobsConf()
conf$mail.start = conf$mail.done = conf$mail.error = "none"

#conf$cluster.functions = makeClusterFunctionsInteractive()
#doExternalTest(whitespace=FALSE)

#conf$cluster.functions = makeClusterFunctionsLocal()
#doExternalTest(whitespace=FALSE)

#conf$cluster.functions = makeClusterFunctionsMulticore()
#doExternalTest(whitespace=FALSE)
#doKillTest(long="sleep")
#doKillTest(long="expensive")

#conf$cluster.functions = makeClusterFunctionsSSH(
#  makeSSHWorker("calcit"), 
#  makeSSHWorker("albit"))
#doExternalTest(whitespace=FALSE)
#doKillTest()

conf$cluster.functions = makeClusterFunctionsLSF("/home/bischl/batchjobs/BatchJobs/examples/cfLSF/ls11.tmpl")
doExternalTest(whitespace=FALSE, sleep.master=15)
doKillTest()

