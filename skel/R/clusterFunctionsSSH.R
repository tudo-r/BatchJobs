## SSH cluster functions.
findSSHHelper = function(nodename, rhome) {
  cmd = "'\"%s/bin/Rscript\" -e \"message(system.file(\\\"bin/ssh-helper\\\", package=\\\"BatchJobs\\\"))\"'"
  res = system2("ssh", c(nodename, sprintf(cmd, rhome)),
    stdout=TRUE, stderr=TRUE)
  res[[1L]]
}

onSSHWorker = function(worker, command) {
  cmd = paste(c(worker$ssh_helper, worker$rhome, command), collapse=" ")
  args = c(worker$nodename, paste("'", cmd, "'", sep=""))
  res = system2("ssh", args, stdout=TRUE, stderr=TRUE)
  tryCatch(eval(parse(text=paste(res, collapse="\n"))),
    error=function(x) stop(res))
}

#' @title Create SSH worker for SSH cluster functions.
#' @param nodename [\code{character(1)}]\cr
#'   Host name of node.
#' @param rhome [\code{character(1)}]\cr
#'   Path to R installation on worker.
#'   Default is \code{R.home()}. Note that the default value created on the master!
#' @param ncpus [\code{integers(1)}]\cr
#'   Number of VPUs of worker.
#'   Default is means to query the worker via \dQuote{/proc/cpuinfo}.
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
  checkArg(nodename, "character", len=1L, na.ok=FALSE)
  checkArg(rhome, "character", len=1L, na.ok=FALSE)
    
  worker = structure(list(
      nodename = nodename,
      rhome = rhome,
      last_update = 0, # Last time load was updated
      ssh_helper = findSSHHelper(nodename, rhome)
    ), class="SSHWorker")
  
  if (missing(ncpus)) {
    ncpus = onSSHWorker(worker, "number-of-processors")
    messagef("Setting for worker %s ncpus=%i", worker$nodename, ncpus)
  } else {
    ncpus = convertInteger(ncpus)
    checkArg(ncpus, "integer", len=1L, na.ok=FALSE)
  }
  worker$ncpus = ncpus
  
  if (missing(max.load)) { 
    max.load = ncpus-1
  } else {
    checkArg(max.load, "numeric", len=1L, na.ok=FALSE)
  }
  if (max.load > ncpus)
    stop("max.load must be <= ncpus!")
  worker$max.load = max.load
  
  if (missing(max.jobs)) {
    max.jobs = ncpus
  } else {
    max.jobs = convertInteger(max.jobs)
    checkArg(max.jobs, "integer", len=1L, na.ok=FALSE)
  }
  if (max.jobs > ncpus)
    stop("max.jobs must be <= ncpus!")
  worker$max.jobs = max.jobs
  
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
  checkListElementClass(workers, "SSHWorker")
  nodenames = extractSubList(workers, "nodename") 
  if (any(duplicated(nodenames)))
    stop("Multiple definitions for a worker nodename!")
  names(workers) = nodenames
  
  isWorkerBusy = function(worker) {
    # should not have too many R sessions open
    (worker$status$running_r_processes > 3 * worker$ncpus) ||
      # should not have too much load average
      (worker$status$load[1L] > worker$max.load) ||
      # there are already ncpus expensive R jobs running on the node
      (worker$status$running_r_processes_50 >= worker$ncpus) ||
      # we have already used up our maximal load on this node
      (worker$status$running_batch_jobs >= worker$max.jobs)
    # else all clear, submit the job!
  }
  
  updateWorker = function(reg, worker) {        
    worker$last_update = as.integer(Sys.time())
    worker$status = onSSHWorker(worker, c("worker-status", reg$file.dir))
    return(worker)
  }
  
  findWorker = function(reg) {
    for (i in seq_along(workers)) {
      worker = workers[[i]]
      time = as.integer(Sys.time())
      # each worker is touched maximally once in 5 secs
      if (time - worker$last_update > 5) {
        worker = updateWorker(reg, worker)
        if (!isWorkerBusy(worker))
          return(worker)
      }
    }
    return(NULL)
  }
  
  submitJob = function(reg, job.name, rscript, log.file, job.dir, resources) {
    worker = findWorker(reg)
    if (is.null(worker)) {
      makeSubmitJobResult(status=1L, batch.job.id=NULL, msg="No free worker available")
    } else {
      pid = try(onSSHWorker(worker,
          c("start-job",  job.dir, rscript, log.file)))
      if (inherits(pid, "try-error")) {
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
    worker = workers[[nodename]]
    if (is.null(worker))
      stop("Unknown worker node '", nodename, "'.")
    onSSHWorker(worker, c("kill-job", pid))
  }
  
  listJobs = function(reg) {
    res = NULL
    for (worker in workers) {
      nodename = worker[["nodename"]]
      pids = onSSHWorker(worker, c("running-jobs"))
      if (length(pids) > 0L) {
        res = c(res, paste(nodename, "#", pids, sep=""))
      }
    }
    res
  }
  
  makeClusterFunctions("SSH", submitJob=submitJob, killJob=killJob, listJobs=listJobs)
} 
