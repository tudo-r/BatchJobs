#' @title Create cluster functions for LSF systems.
#'
#' @description
#' Job files are created based on the brew template
#' \code{template.file}. This file is processed with brew and then
#' submitted to the queue using the \code{bsub} command. Jobs are
#' killed using the \code{bkill} command and the list of running jobs
#' is retrieved using \code{bjobs -u $USER -w}. The user must have the
#' appropriate privileges to submit, delete and list jobs on the
#' cluster (this is usually the case).
#'
#' The template file can access all arguments passed to the
#' \code{submitJob} function, see here \code{\link{ClusterFunctions}}.
#' It is the template file's job to choose a queue for the job
#' and handle the desired resource allocations.
#' Examples can be found on
#' \url{https://github.com/tudo-r/BatchJobs/tree/master/examples/cfLSF}.
#'
#' @template arg_template
#' @template arg_list_jobs_cmd
#' @template ret_cf
#' @family clusterFunctions
#' @export
makeClusterFunctionsLSF = function(template.file, list.jobs.cmd = c("bjobs", "-u $USER", "-w")) {
  assertCharacter(list.jobs.cmd, min.len = 1L, any.missing = FALSE)
  template = cfReadBrewTemplate(template.file)

  # When LSB_BJOBS_CONSISTENT_EXIT_CODE = Y, the bjobs command exits with 0 only
  # when unfinished jobs are found, and 255 when no jobs are found,
  # or a non-existent job ID is entered.
  Sys.setenv(LSB_BJOBS_CONSISTENT_EXIT_CODE = "Y")

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    outfile = cfBrewTemplate(conf, template, rscript, "job")
    # returns: "Job <128952> is submitted to default queue <s_amd>."
    res = runOSCommandLinux("bsub", stdin = outfile, stop.on.exit.code = FALSE)
    # FIXME filled queues
    if (res$exit.code > 0L) {
      cfHandleUnknownSubmitError("bsub", res$exit.code, res$output)
    } else {
      # collapse output strings and first number in string is batch.job.id
      batch.job.id = str_extract(collapse(res$output, sep = " "), "\\d+")
      makeSubmitJobResult(status = 0L, batch.job.id = batch.job.id)
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("bkill", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # JOBID   USER    STAT  QUEUE      FROM_HOST   EXEC_HOST   JOB_NAME   SUBMIT_TIME
    # 106560  rogon   UNKWN m_amd      hpc84       hpc25       QScript    Mar 19 12:18
    # res = runOSyyCommandLinux("bjobs", c("-u $USER", "-w"), stop.on.exit.code = FALSE)
    res = runOSCommandLinux(list.jobs.cmd[1L], list.jobs.cmd[-1L], stop.on.exit.code = FALSE)
    if (res$exit.code == 255L && grepl("No unfinished job found", res$output, fixed = TRUE))
      return(character(0L))
    if (res$exit.code > 0L)
      stopf("bjobs produced exit code %i; output %s", res$exit.code, res$output)

    # drop first header line of output
    out = tail(res$output, -1L)
    # first number in strings are batch.job.ids
    str_extract(out, "\\d+")
  }

  getArrayEnvirName = function() "LSB_JOBINDEX"

  makeClusterFunctions(name = "LSF", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
