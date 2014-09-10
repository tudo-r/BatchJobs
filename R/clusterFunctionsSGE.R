#' @title Create cluster functions for Sun Grid Engine systems.
#'
#' @description
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
#' Examples can be found on
#' \url{https://github.com/tudo-r/BatchJobs/tree/master/examples/cfSGE}.
#'
#' @template arg_template
#' @template arg_list_jobs_cmd
#' @template ret_cf
#' @family clusterFunctions
#' @export
makeClusterFunctionsSGE = function(template.file, list.jobs.cmd = c("qstat",  "-u $USER")) {
  assertCharacter(list.jobs.cmd, min.len = 1L, any.missing = FALSE)
  template = cfReadBrewTemplate(template.file)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    outfile = cfBrewTemplate(conf, template, rscript, "job")
    # returns: "Your job 240933 (\"sleep 60\") has been submitted"
    res = runOSCommandLinux("qsub", outfile, stop.on.exit.code = FALSE)
    # FIXME filled queues
    if (res$exit.code > 0L) {
      cfHandleUnknownSubmitError("qsub", res$exit.code, res$output)
    } else {
      # collapse output strings and first number in string is batch.job.id
      batch.job.id = str_extract(collapse(res$output, sep = " "), "\\d+")
      makeSubmitJobResult(status = 0L, batch.job.id = batch.job.id)
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
    # res = runOSCommandLinux("qstat", "-u $USER")
    res = runOSCommandLinux(list.jobs.cmd[1L], list.jobs.cmd[-1L])

    # drop first 2 header lines
    out = tail(res$output, -2L)
    # first number in strings are batch.job.ids
    str_extract(out, "\\d+")
  }

  getArrayEnvirName = function() "SGE_TASK_ID"

  makeClusterFunctions(name = "SGE", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
