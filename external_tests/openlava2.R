library(BatchJobs)

conf = BatchJobs:::getBatchJobsConf()
conf$cluster.functions = makeClusterFunctionsLSF("/home/clusteradmin/BatchJobs/examples/cfOpenLava/openlava.tmpl")

reg = makeRegistry(id = "BatchJobsExampleOpenLava")
f = function(x) x^2
batchMap(reg, f, 1:2)
submitJobs(reg)
showStatus(reg)
waitForJobs(reg)
showStatus(reg)
