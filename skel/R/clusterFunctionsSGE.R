#' Create cluster functions for Sun Grid Engine systems.
#'
#' Job files are created based on the brew template
#' \code{template.file}. This file is processed with brew and then
#' submitted to the queue using the \code{qsub} command. Jobs are
#' killed using the \code{qdel} command and the list of running jobs
#' is retrieved using \code{qselect}. The user must have the
#' appropriate privileges to submit, delete and list jobs on the
#' cluster (this is usually the case).
#'
#' The template file can access all arguments passed to the
#' \code{submitJob} function, see here \code{\link{ClusterFunctions}}.
#' It is the template file's job to choose a queue for the job
#' and handle the desired resource allocations.
#' A simple example is provided here
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSGE/simple.tmpl}
#' in the package repository on its homepage.
#'
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the job file.
#' @return [\code{\link{ClusterFunctions}}].
#' @examples
#' \dontrun{
#' cluster.functions = makeClusterFunctionsSGE("~/mytemplate.tmpl")
#' }
#' @export
#' @seealso \link{ClusterFunctions}
makeClusterFunctionsSGE = function(template.file) {
  template = cfReadBrewTemplate(template.file)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    outfile = cfBrewTemplate(conf, template, rscript, "job")
    # returns: "Your job 240933 (\"sleep 60\") has been submitted"
    res = runOSCommandLinux("qsub", outfile, stop.on.exit.code=FALSE)
    # FIXME filled queues
    if (res$exit.code > 0L) {
      cfHandleUnkownSubmitError("qsub", res$exit.code, res$output)
    } else {
      # collapse output strings and first number in string is batch.job.id
      batch.job.id = strextract(collapse(res$output, sep=" "), "\\d+")
      makeSubmitJobResult(status=0L, batch.job.id=batch.job.id)
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("qdel", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # looks like this
    # job-ID  prior   name       user         state submit/start at     queue                          slots ja-task-ID
    #-----------------------------------------------------------------------------------------------------------------
    #  240935 0.00000 sleep 60   matthias     qw    04/03/2012 15:45:54                                    1
    res = runOSCommandLinux("qstat", "-u $USER")
    if (res$exit.code > 0L)
      stopf("qstat produced exit code %i; output %s", res$exit.code, res$output)

    # drop first 2 header lines
    out = tail(res$output, -2L)
    # first number in strings are batch.job.ids
    strextract(out, "\\d+")
  }

  makeClusterFunctions(name="SGE", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
