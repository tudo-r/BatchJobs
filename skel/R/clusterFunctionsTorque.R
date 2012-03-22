#' Cluster function implementation for Torque/PBS batch system. Job
#' files are created based on the brew template
#' \code{template.file}. This file is processed with brew and then
#' submitted to the queue using the \code{qsub} command. Jobs are
#' killed using the \code{qdel} command and the list of running jobs
#' is retrieved using \code{qselect}. The user must have the
#' appropriate privileges to submit, delete and list jobs on the
#' cluster (this is usually the case).
#'
#' The template file can use any of the resources defined for the job
#' by accessing the appropirate element of the \code{resources}
#' list. It is the template file's job to choose a queue for the job
#' and add any desired resource allocations. An example of a fairly
#' complex template is provided on the package home page.
#' 
#' @title Create cluster functions for torque-based systems.
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the PBS job file.
#' @param pbs.file.in.temp.dir [\code{logical(1)}]\cr
#'   Should PBS file be created in temp dir and automatically be cleaned up?
#'   Or place it in the jobs directory where the R script and result are put? 
#'   Mainly for debugging purposes.
#'   Default is \code{TRUE}.
#' @return [\code{\link{ClusterFunctions}}]. 
#' @export
makeClusterFunctionsTorque = function(template.file, pbs.file.in.temp.dir=TRUE) {
  ## Read in template
  fd = file(template.file, "r")
  template = paste(readLines(fd), collapse="\n")
  close(fd)
  
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    if (pbs.file.in.temp.dir) { 
      outfile = tempfile()
    } else {
      # if not temp, use jobs dir
      outfile = str_replace(rscript, "\\.R$", ".pbs")
    }
    brew(text=template, output=outfile)
    # get qsub filled queue err msg
    cmd = sprintf("qsub '%s' 2>&1", outfile)
    # suppress warnings ala "running command 'qsub '/tmp/RtmphBHQfx/file1c2936e0' 2>&1' had status 226"
    res = suppressWarnings(system(cmd, intern=TRUE, wait=TRUE))
    max.jobs.msg = "Maximum number of jobs already in queue"
    if (str_detect(res, max.jobs.msg))
      makeSubmitJobResult(status=1L, batch.job.id=as.character(NA), msg=max.jobs.msg)
    else
      makeSubmitJobResult(status=0L, batch.job.id=res)
  }
  
  killJob = function(reg, batch.job.id) {
    # qdel sends SIGTERM, delay, SIGKILL
    cmd = sprintf("qdel '%s'", batch.job.id)
    suppressAll(system(cmd, intern=TRUE, wait=TRUE, 
      ignore.stderr=TRUE, ignore.stdout=TRUE))
  }
  
  listJobs = function(reg) {
    cmd = "qselect -u $USER"
    # Result is lines of fully quantified batch.job.ids
    jobs = system(cmd, intern=TRUE, wait=TRUE)
    jobs
  }
  
  makeClusterFunctions(name="Torque", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
