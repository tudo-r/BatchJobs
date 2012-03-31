#' Use multiple cores on local Linux machine to spawn parallel jobs.
#' 
#' @param ncpus [\code{integers(1)}]\cr
#'   Number of VPUs of worker.
#'   Default means to query the worker via \dQuote{/proc/cpuinfo}.
#' @param max.jobs [\code{integer(1)}]\cr
#'   Maximal number of jobs that can run concurrently for the current registry.
#'   Default is \code{ncpus}.
#' @param max.load [\code{numeric(1)}]\cr
#'   Load average (of the last 5 min) at which the worker is considered occupied, 
#'   so that no job can be submitted. 
#'   Default is \code{ncpus-1}.
#' @param script [\code{character(1)}]\cr
#'   Path to helper bash script which interacts with the worker.
#'   You really should not have to touch this, as this would imply that we have screwed up and 
#'   published an incompatible version for your system.
#'   This option is only provided as a last resort for very experienced hackers.
#'   This is what is done in the package:
#'   \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/skel/inst/bin/linux-helper}
#'   Default means to take it from package directory.
#' @return [\code{\link{ClusterFunctions}}].
#' @export
makeClusterFunctionsMulticore = function(ncpus, max.jobs, max.load, script) {
  w = makeWorkerLocalLinux(script, ncpus, max.jobs, max.load)
  w$last.update = 0
  workers = list(localhost=w)
  worker.env = new.env()  
  worker.env$workers = workers
  
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    worker = findWorker(worker.env, reg$file.dir)
    if (is.null(worker)) {
      makeSubmitJobResult(status=1L, batch.job.id=NULL, msg="No free core available")
    } else {
      pid = try(startWorkerJob(worker, rscript, log.file))
      if (is.error(pid)) {
        makeSubmitJobResult(status=101L, batch.job.id=NULL, msg="submit failed.")
      } else {
        makeSubmitJobResult(status=0L, batch.job.id=pid)
      }
    }
  }
  
  killJob = function(reg, batch.job.id) {
    killWorkerJob(worker.env$workers[[1]], batch.job.id)
  }
  
  listJobs = function(reg) {
    listWorkerJobs(worker.env$workers[[1]], reg$file.dir)
  }
  
  makeClusterFunctions("Multicore", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
} 
