#' Create cluster functions for synchronous execution in same session.
#'
#' All jobs executed under these cluster functions are executed
#' synchronously, in the same interactive R process that you currently are.
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
makeClusterFunctionsInteractive = function() {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    suppressAll(sys.source(rscript, envir=new.env(), keep.source=FALSE))
    makeSubmitJobResult(status=0L, batch.job.id="cfInteractive", msg="")
  }
  makeClusterFunctions(name="Interactive", submitJob=submitJob, killJob=NULL, listJobs=NULL)
}
