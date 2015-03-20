library(BatchJobs)

conf = BatchJobs:::getBatchJobsConf()

conf$cluster.functions = makeClusterFunctionsOpenLava("rmpi-batch.tmpl")

reg = makeRegistry(id = "BatchJobsRmpiExample")

f = function(x) {
  library("Rmpi")
  
  slaveno <- mpi.universe.size() - 1
  if (slaveno < 1) {
    slaveno <- 1
  }
  
  mpi.spawn.Rslaves(nslaves=slaveno)
  mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))
  mpi.close.Rslaves()
  
}
batchMap(reg, f, 1)
submitJobs(reg, np=2)
showStatus(reg)
waitForJobs(reg)
showStatus(reg)
