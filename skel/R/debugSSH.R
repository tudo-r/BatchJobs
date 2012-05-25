#' Helper function to debug SSH mode.
#'
#' Useful in case of configuration problems.
#' Tries different operations of increasing difficulty
#' and provides debug output on the console.
#'
#' @param nodenames [\code{character}]\cr
#'   Nodes on which workers should be constructed for the test.
#' @param rhome [\code{character}]\cr
#'   Paths to R installation on the workers.
#'   Must be have same length and order as \code{nodenames}.
#'   Length 1 is also allowed, which means that the path is the same for all nodes.
#'   \dQuote{} means R installation on the PATH is used,
#'   of course this implies that it must be on the PATH
#'   (also for non-interactive shells)!
#'   Default is \dQuote{}. 
#' @param dir [\code{character(1)}]\cr
#'   Path where internally used test registries can be created.
#'   Note that this must be shared by all workers.
#'   Default is current working directory.
#' @return Nothing.
#' @export
debugSSH = function(nodenames, rhome="", dir=getwd()) {
  checkArg(nodenames, "character", na.ok=FALSE)
  checkArg(rhome, "character", na.ok=FALSE)
  checkArg(dir, "character", len=1L, na.ok=FALSE)
  n = length(nodenames)
  if (length(rhome) == 1)
    rhome = rep(rhome, n)
  conf = getBatchJobsConf()
  conf$debug = TRUE
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  wd = dir

  messagef("*** System info on master: ***")
  print(Sys.info())
  catf("\n")

  messagef("*** which R on slaves: ***")
  res = sapply(nodenames, runOSCommandLinux, cmd="which", args="R", ssh=TRUE, stop.on.exit.code=TRUE)
  messagef("which R result:")
  print(res)
  catf("\n")

  messagef("*** Find helper script on slaves: ***")
  res = Map(findHelperScriptLinux, rhome, nodenames, ssh=TRUE)
  res = unlist(res); names(res) = nodenames
  messagef("Find helper script result:")
  print(res)
  catf("\n")

  messagef("*** Auto-detecting ncpus for slaves: ***")
  workers = Map(makeWorkerRemoteLinux, nodenames, rhome, ncpus=1)
  res = lapply(workers, runWorkerCommand, command="number-of-cpus")
  messagef("Auto-detecting ncpus result:")
  print(res)
  catf("\n")
  
  queryWorkerStatus = function() {
    messagef("*** Query worker status: ***")
    res = lapply(workers, runWorkerCommand, command="status", args="")
    messagef("Query worker status result:")
    message("load n.rprocs n.rprocs.50 n.jobs")
    print(res)
    catf("\n")
  }
  
  queryWorkerStatus()

  messagef("*** Submitting 1 job: ***")
  ssh.workers = Map(makeSSHWorker, nodenames, rhome)
  conf$cluster.functions = do.call(makeClusterFunctionsSSH, ssh.workers)
  id = "debug_ssh_1"
  reg = makeRegistry(id=id, file.dir=file.path(dir, id), work.dir=wd, sharding=FALSE)
  batchMap(reg, identity, 1)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("Submitting 1 job result: %i", loadResult(reg, 1))
  queryWorkerStatus()

  messagef("*** Killing 2 jobs: ***")
  id = "debug_ssh_2"
  reg = makeRegistry(id=id, file.dir=file.path(dir, id), work.dir=wd, sharding=FALSE)
  f = function(i) if(i <= 1) i else f(i-1) + f(i-2)
  xs = 50 + seq(1,2)
  ids = 1:2
  batchMap(reg, f, xs)
  submitJobs(reg)
  Sys.sleep(3)
  queryWorkerStatus()
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  killJobs(reg, ids)
  queryWorkerStatus()
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  catf("\n")
}
