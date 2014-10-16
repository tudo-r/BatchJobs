#' @title Create cluster functions for torque-based systems.
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
#' \url{https://github.com/tudo-r/BatchJobs/tree/master/examples/cfTorque}.
#'
#' @template arg_template
#' @template arg_list_jobs_cmd
#' @template ret_cf
#' @family clusterFunctions
#' @export
makeClusterFunctionsTorque = function(template.file, list.jobs.cmd = c("qselect", "-u $USER", "-s EHQRTW")) {
  assertCharacter(list.jobs.cmd, min.len = 1L, any.missing = FALSE)
  template = cfReadBrewTemplate(template.file)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    outfile = cfBrewTemplate(conf, template, rscript, "pbs")
    res = runOSCommandLinux("qsub", outfile, stop.on.exit.code = FALSE)

    max.jobs.msg = "Maximum number of jobs already in queue"
    output = collapse(res$output, sep = "\n")
    if (grepl(max.jobs.msg, output, fixed = TRUE)) {
      makeSubmitJobResult(status = 1L, batch.job.id = NA_character_, msg = max.jobs.msg)
    } else if (res$exit.code > 0L) {
      cfHandleUnknownSubmitError("qsub", res$exit.code, res$output)
    } else {
      makeSubmitJobResult(status = 0L, batch.job.id = str_trim(output))
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("qdel", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # Result is lines of fully quantified batch.job.ids
    batch.ids = runOSCommandLinux(list.jobs.cmd[1L], list.jobs.cmd[-1L])$output
    # simplify batch ids of array jobs, i.e. remove the array id from the batch id
    unique(gsub("\\[[[:digit:]]\\]", "[]", batch.ids))
  }

  getArrayEnvirName = function() "PBS_ARRAYID"

  makeClusterFunctions(name = "Torque", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
