library(BatchJobs)
source("helpers.R")

conf = BatchJobs:::getBatchJobsConf()
conf$mail.start = conf$mail.done = conf$mail.error = "none"

conf$cluster.functions = makeClusterFunctionsInteractive()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsLocal()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsTorque("~/batchjobs/examples/cfLSF/simple.tmpl")
doExternalTest(whitespace=FALSE, sleep.master=15)
doKillTest()
