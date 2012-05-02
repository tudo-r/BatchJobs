#' Helper function to debug SSH mode.
#'
#' Useful in case of severe errors. 
#' Tries different operations of increasing difficulty 
#' and provides debug output on the console
#'
#' @param nodenames [\code{character}]\cr
#'   Nodes on which workers should be constructed for the test.
#' @param rhome [\code{character}]\cr
#'   Paths to R installation on the workers. 
#'   Must be have same length and order as \code{nodenames}.
#'   Length 1 is also allowed, which means that the path is the same for all nodes.
#'   Default is \code{R.home()}.
#' @return Nothing.
#' @export
debugSSH = function(nodenames, rhome=R.home()) {
  checkArg(nodenames, "character", na.ok=FALSE)
  checkArg(rhome, "character", na.ok=FALSE)
  n = length(nodenames)
  if (length(rhome) == 1)
    rhome = rep(rhome, n)
  conf = getBatchJobsConf()
  conf$debug = TRUE
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  
  messagef("*** System info: ***")
  print(Sys.info())
  catf("\n") 
  
  messagef("*** which R: ***")
  res = sapply(nodenames, runCommand, cmd="which", args="R", ssh=TRUE, stop.on.exit.code=TRUE)
  messagef("which R result:")
  print(res) 
  catf("\n") 
  
  messagef("*** Find helper script: ***")
  res = Map(findHelperScriptLinux, rhome, nodenames, ssh=TRUE)
  res = unlist(res); names(res) = nodenames
  messagef("Find helper script result:")
  print(res) 
  catf("\n") 
  
  messagef("*** Auto-detecting ncpus: ***")
  workers = Map(makeWorkerRemoteLinux, nodenames, rhome, ncpus=1)
  res = lapply(workers, onWorkerLinux, command="number-of-cpus") 
  messagef("Auto-detecting ncpus result:") 
  print(res) 
  catf("\n") 
  
  messagef("*** Query worker status: ***")
  res = lapply(workers, onWorkerLinux, command="status", args="") 
  messagef("Query worker status result:")
  print(res) 
  catf("\n") 
  
  messagef("*** Submitting 1 job: ***")
  ssh.workers = Map(makeSSHWorker, nodenames, rhome)
  conf$cluster.functions = do.call(makeClusterFunctionsSSH, ssh.workers)
  fd = tempfile()
  reg = makeRegistry(id = "debug_multicore", file.dir=fd, sharding=FALSE)
  batchMap(reg, identity, 1)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("Submitting 1 job result: %i", loadResult(reg, 1))
  messagef("*** Query worker status: ***")
  res = lapply(workers, onWorkerLinux, command="status", args=reg$file.dir) 
  messagef("Query worker status result:")
  print(res) 
  catf("\n") 
  
  messagef("*** Killing 2 jobs: ***")
  fd = tempfile()
  reg = makeRegistry(id = "debug_multicore", file.dir=fd, sharding=FALSE)
  f = function(i) if(i <= 1) i else f(i-1) + f(i-2)
  xs = 50 + seq(1,2)
  ids = 1:2
  batchMap(reg, f, xs)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("*** Query worker status: ***")
  res = lapply(workers, onWorkerLinux, command="status", args=reg$file.dir) 
  messagef("Query worker status result:")
  print(res) 
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  killJobs(reg, ids)
  messagef("*** Query worker status: ***")
  res = lapply(workers, onWorkerLinux, command="status", args=reg$file.dir) 
  messagef("Query worker status result:")
  print(res) 
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  catf("\n") 
}