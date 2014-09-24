#' Create cluster functions for sequential execution in same session.
#'
#' @description
#' All jobs executed under these cluster functions are executed
#' sequentially, in the same interactive R process that you currently are.
#' That is, \code{submitJob} does not return until the
#' job has finished. The main use of this \code{ClusterFunctions}
#' implementation is to test and debug programs on a local computer.
#'
#' Listing jobs returns an empty vector (as no jobs can be running when you call this)
#' and \code{killJob} returns at once (for the same reason).
#'
#' @param write.logs [\code{logical(1)}]\cr
#'   Sink the output to log files. Turning logging off can increase the speed of
#'   calculations but makes it next to impossible to debug.
#'   Default is \code{TRUE}.
#' @return [\code{\link{ClusterFunctions}}].
#' @family clusterFunctions
#' @export
makeClusterFunctionsInteractive = function(write.logs = TRUE) {
  assertFlag(write.logs)

  submitJob = if(write.logs) {
    function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
      # open log file for writing
      fn = file(log.file, open = "wt")
      sink(fn, type = "output")
      sink(fn, type = "message")
      on.exit({
        sink(NULL, type = "output")
        sink(NULL, type = "message")
        close(fn)
      })

      # sink both output and message streams
      try(sys.source(rscript, envir = new.env(), keep.source = FALSE))

      # return job result (always successful)
      makeSubmitJobResult(status = 0L, batch.job.id = "cfInteractive", msg = "")
    }
  } else {
    function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
      suppressAll(try(sys.source(rscript, envir = new.env(), keep.source = FALSE)))
      makeSubmitJobResult(status = 0L, batch.job.id = "cfInteractive", msg = "")
    }
  }

  killJob = function(conf, reg, batch.job.id) NULL

  listJobs = function(conf, reg) integer(0L)

  getArrayEnvirName = function() NA_character_

  makeClusterFunctions(name = "Interactive", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
