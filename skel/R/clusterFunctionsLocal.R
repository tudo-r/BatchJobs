#' Create cluster functions for synchronous execution on local host.
#' 
#' All jobs executed under these cluster functions are executed
#' synchronously, but in an independent, new R session.
#' That is, \code{submitJob} does not return until the
#' job has finished. The main use of this \code{ClusterFunctions}
#' implementation is to test and debug programs on a local computer.
#'
#' Both killing and listing jobs is not supported by this
#' implementation since at any one time there can be only one running
#' job and while it is running, the master R process is blocked.
#'
#' @return [\code{\link{ClusterFunctions}}]. 
#' @export
makeClusterFunctionsLocal = function() {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    # nothing should be on all 3 streams except maybe a segfault. throw away.
    cmd = sprintf("%s CMD BATCH --no-save --no-restore '%s' '%s' > /dev/null 2> /dev/null < /dev/null",
      file.path(R.home("bin"), "R"), rscript, log.file)
    system(cmd, intern=TRUE, wait=TRUE)    
    makeSubmitJobResult(status=0L, batch.job.id="cfLocal")
  }
  makeClusterFunctions(name="Local", submitJob=submitJob, killJob=NULL, listJobs=NULL)
}
