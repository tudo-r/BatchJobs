# Return number of cores on worker.
# @param worker [\code{\link{Worker}}].
#   Worker. 
# @return [\code{integer(1)}].
getWorkerNumberOfCPUs = function(worker) {
  UseMethod("getWorkerNumberOfCPUs")
}

# Return 4 numbers to describe worker status.
# - load average of last 1 min, as given by e.g. uptime
# - number of R processes by _all_ users
# - number of R processes by _all_ users which have a load of >= 50%
# - number of R processes by current user which match $FILEDIR/jobs in the cmd call of R
# @param worker [\code{\link{Worker}}].
#   Worker. 
# @param file.dir [\code{character(1)}}].
#   File dir of registry. 
# @return [named \code{list} of \code{numeric(1)}].
getWorkerStatus = function(worker, file.dir) {
  UseMethod("getWorkerStatus")
}

# Start a job on worker, probably with R CMD BATCH.  
# @param worker [\code{\link{Worker}}].
#   Worker. 
# @param rfile [\code{character(1)}].
#   Path to R file to execute. 
# @param outfile [\code{character(1)}].
#   Path to log file for R process. 
# @return [\code{character(1)}]. Relevant process id.
startWorkerJob = function(worker, rfile, outfile) {
  UseMethod("startWorkerJob")
}

# Kill a job on worker. Really do it.  
# @param worker [\code{\link{Worker}}].
#   Worker. 
# @param pid [\code{character(1)}].
#   Process id from DB/batch.job.id to kill.
# @return Nothing.
killWorkerJob = function(worker, pid) {
  UseMethod("killWorkerJob")
}

# List all jobs on worker belonging to the current registry.
# @param worker [\code{\link{Worker}}].
#   Worker. 
# @param file.dir [\code{character(1)}}].
#   File dir of registry. 
# @return [\code{character}]. Vector of process ids.
listWorkerJobs = function(worker, file.dir) {
  UseMethod("listWorkerJobs")
}

# initialize some values of worker, potenially query cpus
initWorker = function(worker, script, ncpus, max.jobs, max.load) {
  if (missing(script)) {
    # FIXME dont use linux specific in base class
    worker$script = findHelperScriptLinux(worker$rhome, worker$ssh, worker$nodename)
  } else {
    checkArg(script, "character", len=1L, na.ok=FALSE)
    worker$script = script
  }
  if (missing(ncpus)) {
    worker$ncpus = getWorkerNumberOfCPUs(worker)
    messagef("Setting for worker %s: ncpus=%i", worker$nodename, worker$ncpus)
  } else {
    ncpus = convertInteger(ncpus)
    checkArg(ncpus, "integer", len=1L, na.ok=FALSE)
    worker$ncpus = ncpus
  }  
  if (missing(max.jobs)) {
    worker$max.jobs = worker$ncpus
  } else {
    max.jobs = convertInteger(max.jobs)
    checkArg(max.jobs, "integer", len=1L, na.ok=FALSE)
    if (max.jobs > worker$ncpus)
      stopf("max.jobs must be <= ncpus = %i!", worker$ncpus)
    worker$max.jobs = max.jobs
  }
  if (missing(max.load)) { 
    worker$max.load = worker$ncpus-1
  } else {
    checkArg(max.load, "numeric", len=1L, na.ok=FALSE)
    if (max.load > worker$ncpus)
      stopf("max.load must be <= ncpus = %i!", worker$ncpus)
    worker$max.load = max.load
  }
  return(worker)
}

# is a worker busy, see rules below
getWorkerBusyStatus = function(worker) {
  status = 0L
  # we have already used up our maximal load on this node
  if (worker$status$n.jobs >= worker$max.jobs)
    status = 2L
  # should not have too much load average
  else if (worker$status$load[1L] > worker$max.load)
    status = 3L
  # there are already ncpus expensive R jobs running on the node
  else if (worker$status$n.rprocs.50 >= worker$ncpus)
    status = 4L
  # should not have too many R sessions open
  else if(worker$status$n.rprocs > 3 * worker$ncpus)
    status = 5L
  # else all clear, submit the job!
  return(status)  
}

# update status of worker
updateWorker = function(worker, file.dir) {        
  worker$last.update = as.integer(Sys.time())
  worker$status = getWorkerStatus(worker, file.dir)
  return(worker)
}

# find worker via isBusyWorker and update workers while looking
findWorker = function(worker.env, file.dir) {
  for (i in seq_along(worker.env$workers)) {
    worker = worker.env$workers[[i]]
    time = as.integer(Sys.time())
    # each worker is touched maximally once in 5 secs
    if (time - worker$last.update > 5) {
      worker = updateWorker(worker, file.dir)
      worker.env$workers[[i]] = worker
      s = getWorkerBusyStatus(worker)
      if (s == 0L)
        return(list(status=0L, worker=worker))
      else  
        return(list(status=s, worker=NULL))
    }
  }
  return(list(status=0L, worker=worker))
}
