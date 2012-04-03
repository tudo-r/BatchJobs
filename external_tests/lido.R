library(BatchJobs)
source("helpers.R")

conf = BatchJobs:::getBatchJobsConf()
conf$mail.start = conf$mail.done = conf$mail.error = "none"

conf$cluster.functions = makeClusterFunctionsInteractive()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsLocal()
doExternalTest(whitespace=FALSE)

conf$cluster.functions = makeClusterFunctionsTorque("/home/bischl/lido.tmpl")
doExternalTest(whitespace=FALSE)
doKillTest()

# spam queue
conf$cluster.functions = makeClusterFunctionsTorque("/home/bischl/lido.tmpl")
doExternalTest(whitespace=FALSE, n=50, resources=list(walltime=3*24*60*60),
  sleep.master=100, sleep.job=10)
