#' Create cluster functions for sequential execution on local host.
#'
#' All jobs executed under these cluster functions are executed
#' sequentially, but in an independent, new R session.
#' That is, \code{submitJob} does not return until the
#' job has finished. The main use of this \code{ClusterFunctions}
#' implementation is to test and debug programs on a local computer.
#'
#' Listing jobs returns an empty vector (as no jobs can be running when you call this)
#' and \code{killJob} returns at once (for the same reason).
#'
#' @return [\code{\link{ClusterFunctions}}].
#' @export
makeClusterFunctionsLocal = function() {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    # nothing should be on all 3 streams except maybe a segfault. throw away.
    cmd = sprintf("%s CMD BATCH --no-save --no-restore '%s' '%s' > /dev/null 2> /dev/null < /dev/null",
      file.path(R.home("bin"), "R"), rscript, log.file)
    system(cmd, intern=TRUE, wait=TRUE)
    makeSubmitJobResult(status=0L, batch.job.id="cfLocal")
  }

  killJob = function(conf, reg, batch.job.id) NULL

  listJobs = function(conf, reg) integer(0L)

  getArrayEnvirName = function() NA_character_

  makeClusterFunctions(name="Local", submitJob=submitJob, killJob=killJob,
                       listJobs=listJobs, getArrayEnvirName=getArrayEnvirName)
}
