# For unit tests and not exported.
# All jobs are executed synchronously and some output is suppressed.
makeClusterFunctionsUnitTests = function() {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    ee = new.env(parent=.GlobalEnv)
    suppressAll(sys.source(rscript, envir=ee))
    makeSubmitJobResult(status=0L, batch.job.id="cfUnitTests", msg="")
  }

  killJob = function(conf, reg, batch.job.id) NULL
  listJobs = function(conf, reg) integer(0L)
  getArrayEnvirName = function() NA_character_

  makeClusterFunctions(name="Testing", submitJob=submitJob, killJob=killJob,
                       listJobs=listJobs, getArrayEnvirName=getArrayEnvirName)
}
