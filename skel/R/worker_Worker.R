# generics
getWorkerNumberOfCPUs = function(worker) {
  UseMethod("getWorkerNumberOfCPUs")
}

getWorkerStatus = function(worker, file.dir) {
  UseMethod("getWorkerStatus")
}

startWorkerJob = function(worker, rfile, outfile) {
  UseMethod("startWorkerJob")
}

killWorkerJob = function(worker, pid) {
  UseMethod("killWorkerJob")
}

listWorkerJobs = function(worker, file.dir) {
  UseMethod("listWorkerJobs")
}

# initialize some values of worker, potenially query cpus
initWorker = function(worker, ncpus, max.jobs, max.load) {
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
      if (!isWorkerBusy(worker))
        return(worker)
    }
  }
  return(NULL)
}
