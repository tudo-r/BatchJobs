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
  
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    if (conf$debug) { 
      # if not temp, use jobs dir
      outfile = str_replace(rscript, "\\.R$", ".job")
    } else {
      outfile = tempfile()
    }
    brew(text=template, output=outfile)
    # returns: "Your job 240933 (\"sleep 60\") has been submitted"
    res = runCommand("qsub", outfile)  
    # FIXME filled queues
    if (res$exit.code > 0) {
      msg = sprintf("qsub produced exit code %i; output %s", res$exit.code, res$output)
      makeSubmitJobResult(status=101L, msg=msg)
    } else  {
      # first number in string is batch.job.id
      batch.job.id = str_extract(res, "\\d+")
      makeSubmitJobResult(status=0L, batch.job.id=batch.job.id
    }
  }
  
  killJob = function(conf, reg, batch.job.id) {
    # qdel sends SIGTERM, delay, SIGKILL
    runCommand("qdel", batch.job.id)
  }
  
  listJobs = function(conf, reg) {
    # looks like this
    # job-ID  prior   name       user         state submit/start at     queue                          slots ja-task-ID
    #-----------------------------------------------------------------------------------------------------------------
    #  240935 0.00000 sleep 60   matthias     qw    04/03/2012 15:45:54                                    1
    res = runCommand("qstat", "-u $USER")
    # drop first 2 header lines
    res = res[-(1:2)]
    # first number in strings are batch.job.ids
    str_extract(res, "\\d+")
  }
  
  makeClusterFunctions(name="SGE", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
