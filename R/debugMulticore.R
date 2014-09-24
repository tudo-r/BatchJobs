#' Helper function to debug multicore mode.
#'
#' @description
#' Useful in case of severe errors.
#' Tries different operations of increasing difficulty
#' and provides debug output on the console
#'
#' @param r.options [\code{list}]
#'   Options for R and Rscript, one option per element of the vector,
#'   a la \dQuote{--vanilla}.
#'   Default is \code{c("--no-save", "--no-restore", "--no-init-file", "--no-site-file")}.
#' @return Nothing.
#' @family debug
#' @export
debugMulticore = function(r.options) {
  if (missing(r.options))
    r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file")
  conf = getBatchJobsConf()
  conf$debug = TRUE
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  rhome = R.home()

  messagef("*** System info: ***")
  print(Sys.info())
  catf("\n")

  messagef("*** which R: ***")
  res = runOSCommandLinux("which", "R")
  messagef("which R result: %s", res$output)
  catf("\n")

  messagef("*** Find helper script: ***")
  script = findHelperScriptLinux(rhome = rhome, r.options = r.options)
  messagef("Find helper script result: %s", script)
  catf("\n")

  messagef("*** Auto-detecting ncpus: ***")
  worker = makeWorkerLocalLinux(r.options = r.options, script = script, ncpus = 1)
  ncpus = runWorkerCommand(worker, "number-of-cpus")
  messagef("Auto-detecting ncpus result: %s", ncpus)
  catf("\n")

  messagef("*** Query worker status: ***")
  res = runWorkerCommand(worker, "status", args = "")
  messagef("Query worker status result: %s", res)
  catf("\n")

  messagef("*** Submitting 1 job: ***")
  conf$cluster.functions = makeClusterFunctionsMulticore()
  fd = tempfile()
  reg = makeRegistry(id = "debug_multicore", file.dir = fd, sharding = FALSE)
  batchMap(reg, identity, 1)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("Submitting 1 job result: %i", loadResult(reg, 1))
  messagef("Query worker status:")
  res = runWorkerCommand(worker, "status", args = reg$file.dir)
  messagef("Query worker status result: %s", res)
  catf("\n")

  messagef("*** Killing 2 jobs: ***")
  fd = tempfile()
  reg = makeRegistry(id = "debug_multicore", file.dir = fd, sharding = FALSE)
  f = function(i) if(i <= 1) i else f(i-1) + f(i-2)
  xs = 50 + seq(1,2)
  ids = 1:2
  batchMap(reg, f, xs)
  submitJobs(reg)
  Sys.sleep(3)
  messagef("Query worker status:")
  res = runWorkerCommand(worker, "status", args = reg$file.dir)
  messagef("Query worker status result: %s", res)
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  killJobs(reg, ids)
  messagef("Query worker status:")
  res = runWorkerCommand(worker, "status", args = reg$file.dir)
  messagef("Query worker status result: %s", res)
  messagef("Running jobs: %s", collapse(findRunning(reg)))
  catf("\n")
}
