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
#' The template file can use any of the resources defined for the job
#' by accessing the appropriate element of the \code{resources}
#' list. It is the template file's job to choose a queue for the job
#' and add any desired resource allocations. A simple example
#' is provided here 
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSGE/simple.tmpl}
#' in the package repository on its homepage.
#'
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the job file.
#' @return [\code{\link{ClusterFunctions}}]. 
#' @export
makeClusterFunctionsSGE = function(template.file) {
  ## Read in template
  fd = file(template.file, "r")
  template = paste(readLines(fd), collapse="\n")
  close(fd)
  
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    if (conf$debug) { 
      outfile = tempfile()
    } else {
      # if not temp, use jobs dir
      outfile = str_replace(rscript, "\\.R$", ".job")
    }
    brew(text=template, output=outfile)
    res = runCommand("qsub", outfile)  
    res = system(cmd, intern=TRUE, wait=TRUE)
    # FIXME filled queues
    # FIXME errorhandling
    makeSubmitJobResult(status=1L, batch.job.id=as.character(NA), msg=max.jobs.msg)
  }
  
  killJob = function(reg, batch.job.id) {
    # qdel sends SIGTERM, delay, SIGKILL
    runCommand("qdel", batch.job.id)
  }
  
  listJobs = function(conf, reg) {
    # Result is lines of fully quantified batch.job.ids
    runCommand("qselect", "-u $USER")
  }
  
  makeClusterFunctions(name="SGE", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
