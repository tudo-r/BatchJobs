# Construct a remote worker for a Linux machine via SSH.
# 
# @param nodename [\code{character(1)}]\cr
#   Host name of node.
# @param rhome [\code{character(1)}]\cr
#   Path to R installation on worker.
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
makeWorkerRemoteLinux = function(nodename, rhome, script, ncpus, max.jobs, max.load) {
  checkArg(nodename, "character", len=1L, na.ok=FALSE)
  checkArg(rhome, "character", len=1L, na.ok=FALSE)
  w = structure(list(
    ssh = TRUE,  
    nodename = nodename,    
    rhome = rhome
  ), class=c("WorkerRemoteLinux", "WorkerLinux", "Worker"))
  w = initWorker(w, script, ncpus, max.jobs, max.load)
  return(w)
}
