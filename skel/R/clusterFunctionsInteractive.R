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
# FIXME documentation
#'
#' @return [\code{\link{ClusterFunctions}}].
#' @export
makeClusterFunctionsInteractive = function() {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    # open log file for writing
    fn = file(log.file, open="wt")
    on.exit(close(fn))

    # set warn option to not collect warnings but signal them immedeiatly
    # so they get included in the log file
    prev.warn = getOption("warn")
    on.exit(options(warn = prev.warn, add=TRUE))
    if ("warning.as.error" %in% resources && isTRUE(resources$warning.as.error)) {
      # convert warnings to errros
      options(warn = 2L)
    } else {
      # immedeiatly output warnings
      options(warn = 1L)
    }

    # sink both output and message streams
    sink(fn, type="output")
    sink(fn, type="message")
    try(sys.source(rscript, envir=new.env(), keep.source=FALSE))
    sink(NULL, type="output")
    sink(NULL, type="message")

    # return job result (always successful)
    makeSubmitJobResult(status=0L, batch.job.id="cfInteractive", msg="")
  }

  killJob = function(conf, reg, batch.job.id) NULL
  listJobs = function(conf, reg) integer(0L)

  makeClusterFunctions(name="Interactive", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
