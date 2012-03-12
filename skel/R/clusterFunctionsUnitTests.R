# For unit tests and not exported.
# All jobs are executed synchronously and some output is suppressed.
makeClusterFunctionsUnitTests = function() {
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    ee = new.env(parent=.GlobalEnv)
    owd = getwd()
    setwd(job.dir)    
    suppressAll(sys.source(rscript, envir=ee))
    setwd(owd)
    makeSubmitJobResult(status=0L, batch.job.id="", msg="")
  }
  
  makeClusterFunctions(name="Testing", submitJob=submitJob, killJob=NULL, listJobs=NULL)
}
