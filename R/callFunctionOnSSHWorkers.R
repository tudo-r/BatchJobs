#' Call an arbitrary function on specified SSH workers.
#'
#' @description
#' Calls can be made in parallel or consecutively,
#' the function waits until all calls have finished and
#' returns call results.
#' In consecutive mode the output on the workers can also be shown on the master
#' during computation.
#'
#' Please read and understand the comments for argument \code{dir}.
#'
#' Note that this function should only be used for short administrative
#' tasks or information gathering on the workers, the true work horse for
#' real computation is \code{\link{submitJobs}}.
#'
#' In \code{\link{makeSSHWorker}} various options for load
#' management are possible. Note that these will be
#' ignored for the current call to execute it immediatly.
#'
#' @param nodenames [\code{character}]\cr
#'   Nodenames of workers to call function on.
#'   Only workers which were specified in your
#'   \code{\link{makeClusterFunctionsSSH}} configuration can be used.
#' @param fun [\code{function}]\cr
#'   Function to call on workers.
#' @param ... [any]\cr
#'   Arguments for \code{fun}.
#' @param consecutive [\code{logical(1)}]\cr
#'   Do calls consecutively and always wait until each worker is done.
#'   Default is \code{FALSE}.
#' @param show.output [\code{logical(1)}]\cr
#'   Show output of workers on master during call.
#'   Can be useful to see what is happening.
#'   Can only be used in consecutive mode.
#'   Default is \code{consecutive}.
#' @param simplify [\code{logical(1)}]\cr
#'    Should the result be simplified? See \code{\link{sapply}}.
#'    Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Name results by \code{nodenames}.
#'   Default is \code{TRUE}.
#' @param dir [\code{character(1)}]\cr
#'   Directory under which a temporary registry will be
#'   created in a subdirectory for communication.
#'   This has to be somewhere on the shared
#'   filesystem. The created subdirectory will be cleaned up on exit.
#'   Default is current working directory.
#' @return Results of function calls, either a list or simplified.
#' @export
callFunctionOnSSHWorkers = function(nodenames, fun, ...,
  consecutive = FALSE, show.output = consecutive,
  simplify = TRUE, use.names = TRUE, dir = getwd()) {

  assertCharacter(nodenames, any.missing = FALSE)
  assertFunction(fun)
  assertFlag(consecutive)
  assertFlag(show.output)
  assertFlag(simplify)
  assertFlag(use.names)
  if (!consecutive && show.output)
    stop("show.output = TRUE can only be used in consecutive mode.")

  conf = getBatchJobsConf()
  cf = conf$cluster.functions
  mail.old = c(conf$mail.start, conf$mail.done, conf$mail.error)
  if (cf$name != "SSH")
    stop("callFunctionOnSSHWorkers can only be used in SSH mode!")
  # create dummy registry, we submit our command from this
  regid = sprintf("BatchJobs_callFunctionOnSSHWorkers_%i", as.integer(Sys.time()))
  regdir = file.path(dir, regid)
  # we will change mailing and cluster funs, reset them on exit
  # also kill all still running jobs and remove reg dir
  on.exit({
    conf$cluster.functions = cf
    conf$mail.start = mail.old[1L]
    conf$mail.done = mail.old[2L]
    conf$mail.error = mail.old[3L]
  })
  # no mails during for the following jobs, also get the nodenames ssh workers
  conf$mail.start = conf$mail.done = conf$mail.error = "none"
  workers = environment(cf$submitJob)$workers
  d = setdiff(nodenames, names(workers))
  if(length(d) > 0L)
    stopf("For some nodenames no workers exist: %s", collapse(d))
  workers = workers[nodenames]

  # ignore load constraints
  old.worker.settings = list()
  wsettings = c("ncpus", "max.jobs", "max.load")
  for (wn in names(workers)) {
    w = workers[[wn]]
    old.worker.settings[[wn]] = mget(wsettings, w)
    for (ws in wsettings) w[[ws]] = Inf
  }
  on.exit({
    # reset load settings
    for (wn in names(old.worker.settings)) {
      for (ws in wsettings)
        workers[[wn]][[ws]] = old.worker.settings[[wn]][[ws]]
    }
  }, add = TRUE)

  args = if (consecutive)
    args = list(fun)
  else
    replicate(length(nodenames), fun)
  suppressMessages({
    reg = makeRegistry(regid, file.dir = regdir, sharding = FALSE)
    more.args = list(...)
    batchMap(reg, function(fun, ...) {
      print("###logstart###")
      res = fun(...)
      print("###logend###")
      res
    }, args, more.args = more.args)
  })
  on.exit({
    if (length(findOnSystem(reg)) > 0L)
      killJobs(reg, getJobIds(reg))
    if (file.exists(regdir))
      unlink(regdir, recursive = TRUE)
  }, add = TRUE)
  # read log as char string, get part between stamps and print only new chars
  printLog = function(log.old) {
    log.fn = getLogFiles(reg, 1L)
    log.new = readChar(log.fn, file.info(log.fn)$size)
    j = gregexpr("###logstart###", log.new)[[1L]][1L]
    if (j == -1) {
      # start not found
      log.new = ""
    } else {
      # start found, clip
      log.new = substr(log.new, j+15, nchar(log.new))
      j = gregexpr("###logend###", log.new)[[1L]][1L]
      if (j != -1L) {
        # end found, clip
        log.new = substr(log.new, 1L, j-7L)
      }
    }
    cat(substr(log.new, nchar(log.old)+1L, nchar(log.new)))
    log.new
  }

  mysubmit = function(nodenames, reg) {
    messagef("Calling function on: %s.",
      collapse(nodenames))
    conf$cluster.functions = makeClusterFunctionsSSH(workers = workers)
    capture.output(suppressMessages(submitJobs(reg, getJobIds(reg))))
  }

  # while job not done, sleep and maybe print log
  waitTillJobsDone = function(reg) {
    log.old = ""
    while(length(findOnSystem(reg) > 0L)) {
      if (show.output)
        log.old = printLog(log.old)
      Sys.sleep(1)
    }
    if (show.output) {
      log.old = printLog(log.old)
      cat("\n\n")
    }
  }

  # if error, throw it on master
  checkJobErrors = function(reg, nodenames) {
    errids = findErrors(reg)
    if (length(errids) > 0L) {
      j = errids[1L]
      stopf("Error on %s: %s", nodenames[j], getErrorMessages(reg, j))
    }
  }

  doit = function(reg, nodenames) {
    mysubmit(nodenames, reg)
    waitTillJobsDone(reg)
    checkJobErrors(reg, nodenames)
  }
  if (consecutive) {
    # loop though nodes individually and call function
    results = lapply(nodenames, function(nn) {
      log.old = ""
      doit(reg, nn)
      loadResult(reg, 1L)
    })
  } else {
    doit(reg, nodenames)
    results = loadResults(reg, simplify = FALSE, use.names = FALSE)
  }

  if (use.names)
    names(results) = nodenames
  if (simplify)
    results = simplify2array(results)
  return(results)
}

