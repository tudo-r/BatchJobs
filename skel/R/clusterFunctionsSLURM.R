#' Create cluster functions for SLURM-based systems.
#'
#' Job files are created based on the brew template
#' \code{template.file}. This file is processed with brew and then
#' submitted to the queue using the \code{sbatch} command. Jobs are
#' killed using the \code{scancel} command and the list of running jobs
#' is retrieved using \code{squeue}. The user must have the
#' appropriate privileges to submit, delete and list jobs on the
#' cluster (this is usually the case).
#'
#' The template file can access all arguments passed to the
#' \code{submitJob} function, see here \code{\link{ClusterFunctions}}.
#' It is the template file's job to choose a queue for the job
#' and handle the desired resource allocations.
#' A simple example is provided here
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSLURM/simple.tmpl}
#' and a more complex one here
#' \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSLURM/dortmund_fk_statistik.tmpl}
#' in the package repository on its homepage.
#'
#' @param template.file [\code{character(1)}]\cr
#'   Path to a brew template file that is used for the SLURM job file.
#' @return [\code{\link{ClusterFunctions}}].
#' @export
makeClusterFunctionsSLURM = function(template.file) {
  template = cfReadBrewTemplate(template.file)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    outfile = cfBrewTemplate(conf, template, rscript, "sb")
    res = runOSCommandLinux("sbatch", outfile, stop.on.exit.code=FALSE)

    max.jobs.msg = "sbatch: error: Batch job submission failed: Job violates accounting policy (job submit limit, user's size and/or time limits)"
    output = collapse(res$output, sep="\n")
    if (grepl(max.jobs.msg, output, fixed=TRUE)) {
      makeSubmitJobResult(status=1L, batch.job.id=NA_character_, msg=max.jobs.msg)
    } else if (res$exit.code > 0L) {
      cfHandleUnknownSubmitError("sbatch", res$exit.code, res$output)
    } else {
      makeSubmitJobResult(status=0L, batch.job.id=trim(strsplit(output, split=" ")[[1L]][4L]))
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    cfKillBatchJob("scancel", batch.job.id)
  }

  listJobs = function(conf, reg) {
    # Result is lines of fully quantified batch.job.ids
    runOSCommandLinux("squeue", "-h -o %i -u $USER")$output
  }

  getArrayEnvirName = function() "SLURM_ARRAY_TASK_ID"

  makeClusterFunctions(name="SLURM", submitJob=submitJob, killJob=killJob,
                       listJobs=listJobs, getArrayEnvirName = getArrayEnvirName)
}
