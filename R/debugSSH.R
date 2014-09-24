#' @title Helper function to debug SSH mode.
#'
#' @description
#' Useful in case of configuration problems.
#' Tries different operations of increasing difficulty
#' and provides debug output on the console.
#'
#' Note that this function does not access nor use information specified for
#' your cluster functions in your configuration.
#'
#' @param nodename [\code{character(1)}]\cr
#'   Node on which worker should be constructed for the test.
#' @param rhome [\code{character(1)}]\cr
#'   Path to R installation on the worker.
#'   \dQuote{} means R installation on the PATH is used,
#'   of course this implies that it must be on the PATH
#'   (also for non-interactive shells)!
#'   Default is \dQuote{}.
#' @param r.options [\code{list}]
#'   Options for R and Rscript, one option per element of the vector,
#'   a la \dQuote{--vanilla}.
#'   Default is \code{c("--no-save", "--no-restore", "--no-init-file", "--no-site-file")}.
#' @param dir [\code{character(1)}]\cr
#'   Path where internally used test registries can be created.
#'   Note that this must be shared for the worker.
#'   Default is current working directory.
#' @return Nothing.
#' @family debug
#' @export
debugSSH = function(nodename, rhome = "",
  r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file"),
  dir = getwd()) {

  assertString(nodename)
  assertString(rhome)
  assertString(dir)
  conf = getBatchJobsConf()
  conf$debug = TRUE
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  wd = dir

  messagef("*** System info on master: ***")
  print(Sys.info())
  catf("\n")

  messagef("*** which R on slave: ***")
  res = runOSCommandLinux(cmd = "which", args = "R", ssh = TRUE, nodename = nodename, stop.on.exit.code = TRUE)
  messagef("which R result:")
  print(res)
  catf("\n")

  messagef("*** Find helper script on slave: ***")
  res = findHelperScriptLinux(rhome, r.options, ssh = TRUE, nodename = nodename)
  messagef("Find helper script result:")
  print(res)
  catf("\n")

  messagef("*** Auto-detecting ncpus for slave: ***")
  worker = makeWorkerRemoteLinux(nodename = nodename, rhome = rhome, r.options = r.options, ncpus = 1)
  res = runWorkerCommand(worker = worker, command = "number-of-cpus")
  messagef("Auto-detecting ncpus result:")
  print(res)
  catf("\n")

  queryWorkerStatus = function() {
    messagef("*** Query worker status: ***")
    res = runWorkerCommand(worker = worker, command = "status", args = "")
    messagef("Query worker status result:")
    message("load n.rprocs n.rprocs.50 n.jobs")
    print(res)
    catf("\n")
  }

  queryWorkerStatus()

  messagef("*** Submitting 1 job: ***")
  ssh.workers = list(makeSSHWorker(nodename = nodename, rhome = rhome, r.options = r.options))
  conf$cluster.functions = do.call(makeClusterFunctionsSSH, ssh.workers)
  id = "debug_ssh_1"
  reg = makeRegistry(id = id, file.dir = file.path(dir, id), work.dir = wd, sharding = FALSE)
  batchMap(reg, identity, 1)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("Submitting 1 job result: %i", loadResult(reg, 1))
  queryWorkerStatus()

  messagef("*** Killing 2 jobs: ***")
  id = "debug_ssh_2"
  reg = makeRegistry(id = id, file.dir = file.path(dir, id), work.dir = wd, sharding = FALSE)
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
