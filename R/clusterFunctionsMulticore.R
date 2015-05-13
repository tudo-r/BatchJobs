#' Use multiple cores on local Linux machine to spawn parallel jobs.
#'
#' @description
#' Jobs are spawned by starting multiple R sessions on the commandline
#' (similar like on true batch systems).
#' Packages \code{parallel} or \code{multicore} are not used in any way.
#'
#' @param ncpus [\code{integer(1)}]\cr
#'   Number of VPUs of worker.
#'   Default is to use all cores but one, where total number of cores
#'   "available" is given by option \code{\link[base:options]{mc.cores}}
#'   and if that is not set it is inferred by
#'   \code{\link[parallel]{detectCores}}.
#' @param max.jobs [\code{integer(1)}]\cr
#'   Maximal number of jobs that can run concurrently for the current registry.
#'   Default is \code{ncpus}.
#' @param max.load [\code{numeric(1)}]\cr
#'   Load average (of the last 5 min) at which the worker is considered occupied,
#'   so that no job can be submitted.
#'   Default is \code{ncpus-1}.
#' @param nice [\code{integer(1)}]\cr
#'   Process priority to run R with set via nice. Integers between -20 and 19 are allowed.
#'   If missing, processes are not nice'd and the system default applies (usually 0).
#' @param r.options [\code{character}]
#'   Options for R and Rscript, one option per element of the vector,
#'   a la \dQuote{--vanilla}.
#'   Default is \code{c("--no-save", "--no-restore", "--no-init-file", "--no-site-file")}.
#' @param script [\code{character(1)}]\cr
#'   Path to helper bash script which interacts with the worker.
#'   You really should not have to touch this, as this would imply that we have screwed up and
#'   published an incompatible version for your system.
#'   This option is only provided as a last resort for very experienced hackers.
#'   Note that the path has to be absolute.
#'   This is what is done in the package:
#'   \url{https://github.com/tudo-r/BatchJobs/blob/master/inst/bin/linux-helper}
#'   Default means to take it from package directory.
#' @return [\code{\link{ClusterFunctions}}].
#' @family clusterFunctions
#' @export
#' @importFrom parallel detectCores
makeClusterFunctionsMulticore = function(ncpus = max(getOption("mc.cores", detectCores()) - 1, 1), max.jobs, max.load, nice,
  r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file"), script) {

  if (isWindows())
    stop("ClusterFunctionsMulticore do not work in Windows")

  worker = makeWorkerLocalLinux(r.options, script, ncpus, max.jobs, max.load, nice)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    updateWorker(worker, reg$file.dir, tdiff = 0L)
    s = worker$available
    if (s != "A") {
      makeSubmitJobResult(status = 1L, batch.job.id = NULL, msg = sprintf("Multicore busy: %s", s))
    } else {
      pid = try(startWorkerJob(worker, rscript, log.file))
      if (is.error(pid))
        makeSubmitJobResult(status = 101L, batch.job.id = NULL, msg = "Submit failed.")
      else
        makeSubmitJobResult(status = 0L, batch.job.id = pid)
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    killWorkerJob(worker, batch.job.id)
  }

  listJobs = function(conf, reg) {
    listWorkerJobs(worker, reg$file.dir)
  }

  getArrayEnvirName = function() NA_character_

  makeClusterFunctions(name = "Multicore", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
