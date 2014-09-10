#' Create SSH worker for SSH cluster functions.
#'
#' @param nodename [\code{character(1)}]\cr
#'   Host name of node.
#' @param rhome [\code{character(1)}]\cr
#'   Path to R installation on worker.
#'   \dQuote{} means R installation on the PATH is used,
#'   of course this implies that it must be on the PATH
#'   (also for non-interactive shells)!
#'   Default is \dQuote{}.
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
#' @param nice [\code{integer(1)}]\cr
#'   Process priority to run R with set via nice. Integers between -20 and 19 are allowed.
#'   If missing, processes are not nice'd and the system default applies (usually 0).
#'   Default is no niceing.
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
#' @return [\code{\link{SSHWorker}}].
#' @export
#' @aliases SSHWorker
makeSSHWorker = function(nodename, rhome = "", ncpus, max.jobs, max.load, nice,
  r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file"), script) {

  worker = makeWorkerRemoteLinux(nodename, rhome, r.options, script, ncpus, max.jobs, max.load, nice)
  class(worker) = c("SSHWorker", class(worker))
  return(worker)
}

#' Create an SSH cluster to execute jobs.
#'
#' Worker nodes must share the same file system and be accessible by ssh
#' without manually entering passwords (e.g. by ssh-agent or passwordless pubkey).
#' Note that you can also use this function to parallelize on multiple cores on your local machine.
#' But you still have to run an ssh server and provide passwordless access to
#' localhost.
#'
#' @param ...  [\code{\link{SSHWorker}}]\cr
#'   Worker objects, all created with \code{\link{makeSSHWorker}}.
#' @param workers [list of \code{\link{SSHWorker}}]\cr
#'   Alternative way to pass workers.
#' @return [\code{ClusterFunctions}].
#'
#' @examples \dontrun{
#'
#' # Assume you have three nodes larry, curley and moe. All have 6
#' # cpu cores. On curley and moe R is installed under
#' # "/opt/R/R-current" and on larry R is installed under
#' # "/usr/local/R/". larry should not be used extensively because
#' # somebody else wants to compute there as well.
#' # Then a call to 'makeClusterFunctionsSSH'
#' # might look like this:
#'
#' cluster.functions = makeClusterFunctionsSSH(
#'   makeSSHWorker(nodename = "larry", rhome = "/usr/local/R", max.jobs = 2),
#'   makeSSHWorker(nodename = "curley", rhome = "/opt/R/R-current"),
#'   makeSSHWorker(nodename = "moe", rhome = "/opt/R/R-current"))
#' }
#' @export
#' @family clusterFunctions
#' @seealso \code{\link{makeSSHWorker}}
makeClusterFunctionsSSH = function(..., workers) {
  args = list(...)
  if (!xor(length(args) > 0L, !missing(workers)))
    stop("You must use exactly only 1 of: '...', 'workers'!")
  if (missing(workers))
    workers = args
  checkListElementClass(workers, "SSHWorker")
  if (length(workers) == 0L)
    stop("You must pass at least 1 SSH worker!")
  nodenames = extractSubList(workers, "nodename")
  dup = duplicated(nodenames)
  if (any(dup))
    stopf("Multiple definitions for worker nodenames: %s!", collapse(nodenames[dup]))
  names(workers) = nodenames
  rm(nodenames, dup)

  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    worker = findWorker(workers, reg$file.dir, tdiff = 5L)
    if (is.null(worker)) {
      states = collapse(extractSubList(workers, "available", simplify = TRUE), sep = "")
      makeSubmitJobResult(status = 1L, batch.job.id = NULL,
        msg = sprintf("Workers busy: %s", states))
    } else {
      pid = try(startWorkerJob(worker, rscript, log.file))
      if (is.error(pid))
        makeSubmitJobResult(status = 101L, batch.job.id = NULL, msg = "Submit failed.")
      else
        makeSubmitJobResult(status = 0L,batch.job.id = paste0(worker$nodename, "#", pid))
    }
  }

  killJob = function(conf, reg, batch.job.id) {
    parts = strsplit(batch.job.id, "#", fixed = TRUE)[[1L]]
    nodename = parts[1L]
    pid = parts[2L]
    worker = workers[[nodename]]
    if (is.null(worker))
      stopf("Unknown worker node '%s'.", nodename)
    killWorkerJob(worker, pid)
  }

  listJobs = function(conf, reg) {
    res = NULL
    for (worker in workers) {
      nodename = worker[["nodename"]]
      pids = listWorkerJobs(worker, reg$file.dir)
      if (length(pids) > 0L) {
        res = c(res, paste0(nodename, "#", pids))
      }
    }
    res
  }

  getArrayEnvirName = function() NA_character_

  makeClusterFunctions(name = "SSH", submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
