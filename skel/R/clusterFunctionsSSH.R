#' Create SSH worker for SSH cluster functions.
#' 
#' @param nodename [\code{character(1)}]\cr
#'   Host name of node.
#' @param rhome [\code{character(1)}]\cr
#'   Path to R installation on worker.
#'   Default is \code{R.home()}. Note that the default value created on the master!
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
#' @return [\code{\link{SSHWorker}}].
#' @export
#' @aliases SSHWorker
makeSSHWorker = function(nodename, rhome=R.home(), ncpus, max.jobs, max.load) {
  #FIXME we might want to use autodetection of the R interpreter
  #FIXME be careful with non-interactive sessions, R might then not be in the path!
  worker = makeWorkerRemoteLinux(nodename = nodename, rhome=rhome, 
    ncpus, max.jobs, max.load)
  worker$last.update = 0
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
#' cf = makeClusterFunctionsSSH(
#'   makeSSHWorker(nodename="larry", rhome="/usr/local/R", max.jobs=2),
#'   makeSSHWorker(nodename="curley", rhome="/opt/R/R-current"),
#'   makeSSHWorker(nodename="moe", rhome="/opt/R/R-current"))
#' }
#' @export
makeClusterFunctionsSSH = function(...) {
  workers = list(...)
  checkArg(workers, "list")
  checkListElementClass(workers, "WorkerRemoteLinux")
  nodenames = extractSubList(workers, "nodename") 
  if (any(duplicated(nodenames)))
    stop("Multiple definitions for a worker nodename!")
  names(workers) = nodenames
  worker.env = new.env()  
  worker.env$workers = workers
  
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    worker = findWorker(worker.env, reg)
    if (is.null(worker)) {
      makeSubmitJobResult(status=1L, batch.job.id=NULL, msg="No free worker available")
    } else {
      pid = try(startWorkerJob(worker, job.dir, rscript, log.file))
      if (is.error(pid)) {
        makeSubmitJobResult(status=101L, batch.job.id=NULL, msg="submit failed.")
      } else {
        makeSubmitJobResult(status=0L,
          batch.job.id=paste(worker$nodename, pid, sep="#"))
      }
    }
  }
  
  killJob = function(reg, batch.job.id) {
    parts = str_split_fixed(batch.job.id, "#", 2L)[1L,]
    nodename = parts[1L]
    pid = parts[2L]
    worker = worker.env$workers[[nodename]]
    if (is.null(worker))
      stop("Unknown worker node '", nodename, "'.")
    killWorkerJob(worker, pid)  
  }
  
  listJobs = function(reg) {
    res = NULL
    for (worker in worker.env$workers) {
      nodename = worker[["nodename"]]
      pids = listWorkerJobs(worker)
      if (length(pids) > 0L) {
        res = c(res, paste(nodename, "#", pids, sep=""))
      }
    }
    res
  }
  
  makeClusterFunctions("SSH", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
} 
