#' Create cluster functions for torque-based systems.
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
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfTorque/simple.tmpl}
#' and a more complex one here
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfTorque/lido.tmpl}
#' in the package repository on its homepage.
#'
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the PBS job file.
#' @return [\code{\link{ClusterFunctions}}].
#' @export
#  FIXME document what variables are avial. in brew templ.
makeClusterFunctionsTorque = function(template.file) {
  template = cfReadBrewTemplate(template.file)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources) {
    outfile = cfBrewTemplate(conf, template, rscript)
    res = runOSCommandLinux("qsub", outfile, stop.on.exit.code=FALSE)
    # FIXME this is faulty: qsub can return multiple lines, e.g. in adelaide this happens.
    # maybe we should have an option to concatenate the lines
    # (otherwise the first if goes already wrong)
    # also how to extract the job id in a best way?
    # also LSF and SGE
    # ML: Output always collapsed and trim on result. Unsure if this is sufficient.

    max.jobs.msg = "Maximum number of jobs already in queue"
    output = collapse(res$output, sep="\n")
    if (grepl(max.jobs.msg, output, fixed=TRUE)) {
      makeSubmitJobResult(status=1L, batch.job.id=NA_character_, msg=max.jobs.msg)
    } else if (res$exit.code > 0L) {
      cfHandleUnkownSubmitError("qsub", res)
    } else {
      makeSubmitJobResult(status=0L, batch.job.id=trim(output))
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("qdel", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # Result is lines of fully quantified batch.job.ids
    runOSCommandLinux("qselect", "-u $USER")$output
  }

  makeClusterFunctions(name="Torque", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
}
