# Construct a worker for local Linux machine to spawn parallel jobs.
# 
# @param script [\code{character(1)}]\cr
#   Path to helper script on worker.
#   Default means to take it from package directory.
# @param ncpus [\code{integers(1)}]\cr
#   Number of VPUs of worker.
#   Default means to query the worker via \dQuote{/proc/cpuinfo}.
# @param max.jobs [\code{integer(1)}]\cr
#   Maximal number of jobs that can run concurrently for the current registry.
#   Default is \code{ncpus}.
# @param max.load [\code{numeric(1)}]\cr
#   Load average (of the last 5 min) at which the worker is considered occupied, 
#   so that no job can be submitted. 
#   Default is \code{ncpus-1}.
# @return [\code{\link{WorkerLocalLinux}}].
makeWorkerLocalLinux = function(script, ncpus, max.jobs, max.load) {
  rhome = R.home()
  w = structure(list(
    ssh = FALSE,  
    nodename = "localhost",
    rhome = rhome
  ), class=c("WorkerLocalLinux", "WorkerLinux", "Worker"))
  w = initWorker(w, script, ncpus, max.jobs, max.load)
  return(w)
}
