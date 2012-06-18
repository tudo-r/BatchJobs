#' Create cluster functions for LSF systems.
#'
#' Job files are created based on the brew template
#' \code{template.file}. This file is processed with brew and then
#' submitted to the queue using the \code{bsub} command. Jobs are
#' killed using the \code{bkill} command and the list of running jobs
#' is retrieved using \code{bjobs -u $USER -w}. The user must have the
#' appropriate privileges to submit, delete and list jobs on the
#' cluster (this is usually the case).
#'
#' The template file can use any of the resources defined for the job
#' by accessing the appropriate element of the \code{resources}
#' list. It is the template file's job to choose a queue for the job
#' and add any desired resource allocations. A simple example
#' is provided here
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfLSF/simple.tmpl}
#' in the package repository on its homepage.
#'
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the job file.
#' @return [\code{\link{ClusterFunctions}}].
#' @export
makeClusterFunctionsLSF = function(template.file) {
  template = cfReadBrewTemplate(template.file)
  
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    outfile = cfBrewTemplate(conf, template, rscript)
    # returns: "Job <128952> is submitted to default queue <s_amd>."
    res = system3("bsub", stdin=outfile)
    # FIXME filled queues
    if (res$exit.code > 0L) {
      cfHandleUnkownSubmitError("bsub", res)
    } else {
      # collapse output strings and first number in string is batch.job.id
      batch.job.id = strextract(collapse(res$output, sep=" "), "\\d+", global=FALSE)
      makeSubmitJobResult(status=0L, batch.job.id=batch.job.id)
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("bkill", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # JOBID   USER    STAT  QUEUE      FROM_HOST   EXEC_HOST   JOB_NAME   SUBMIT_TIME
    # 106560  rogon   UNKWN m_amd      hpc84       hpc25       QScript    Mar 19 12:18
    res = runOSCommandLinux("bjobs", c("-u $USER", "-w"), stop.on.exit.code=FALSE)
    if (res$exit.code == 255L && grepl("No unfinished job found", res$output, fixed=TRUE))
      return(character(0L))
    if (res$exit.code > 0L)
      stopf("bjobs produced exit code %i; output %s", res$exit.code, res$output)
    res = res$output
    # drop first header line
    res = res[-1L]
    # first number in strings are batch.job.ids
    strextract(res, "\\d+")
  }

  makeClusterFunctions(name="LSF", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
