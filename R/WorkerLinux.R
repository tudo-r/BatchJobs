# ******************** Constructors ********************

# Construct a remote worker for a Linux machine via SSH.
makeWorkerRemoteLinux = function(nodename, rhome, r.options, script, ncpus, max.jobs, max.load, nice) {
  makeWorker(ssh = TRUE, nodename, rhome, r.options, script, ncpus, max.jobs, max.load, nice, c("WorkerRemoteLinux", "WorkerLinux"))
}

# Construct a worker for local Linux machine to spawn parallel jobs.
makeWorkerLocalLinux = function(r.options, script, ncpus, max.jobs, max.load, nice) {
  makeWorker(ssh = FALSE, "localhost", R.home(), r.options, script, ncpus, max.jobs, max.load, nice, c("WorkerLocalLinux", "WorkerLinux"))
}

# ******************** Interface implementation ********************

#' @export
getWorkerNumberOfCPUs.WorkerLinux = function(worker) {
  as.integer(runWorkerCommand(worker, "number-of-cpus"))
}

#' @export
getWorkerStatus.WorkerLinux = function(worker, file.dir) {
  res = runWorkerCommand(worker, "status", file.dir)
  setNames(as.list(as.numeric(strsplit(res, " +")[[1L]])),
           c("load", "n.rprocs", "n.rprocs.50", "n.jobs"))
}

#' @export
startWorkerJob.WorkerLinux = function(worker, rfile, outfile) {
  runWorkerCommand(worker, "start-job", c(worker$rhome, worker$nice, worker$r.options, rfile, outfile))
}

#' @export
killWorkerJob.WorkerLinux = function(worker, pid) {
  runWorkerCommand(worker, "kill-job", pid)
}

#' @export
listWorkerJobs.WorkerLinux = function(worker, file.dir) {
  res = runWorkerCommand(worker, "list-jobs", file.dir)
  gsub("^\\s+|\\s+$", "", res)
}

# ******************** Run commands on OS ********************

# Runs a command, either by SSH or directly on localhost.
# @param cmd [\code{character(1)}]
#   System command to run.
# @param args [\code{character}]
#   System command arguments.
#   Default is \code{character(0)}.
# @param stdin [\code{character(1)}]
#   See \code{\link{system3}}.
#   Default is \dQuote{}.
# @param stop.on.exit.code [\code{character}]
#   See \code{\link{system3}}.
#   Default is \code{TRUE}.
# @param ssh [\code{logical(1)}]
#   Use SSH?
#   Default is \code{FALSE}.
# @param nodename [\code{character(1)}]
#   Nodename for SSH.
# @return See \code{\link{system3}}.
runOSCommandLinux = function(cmd, args = character(0L), stdin = "", stop.on.exit.code = TRUE, ssh = FALSE, nodename) {
  conf = getBatchJobsConf()
  if (ssh) {
    sys.cmd = "ssh"
    ssh.cmd = sprintf("'%s'", collapse(c(cmd, args), sep = " "))
    sys.args = c(nodename, ssh.cmd)
  } else {
    sys.cmd = cmd
    sys.args = args
  }
  if (conf$debug) {
    catf("OS cmd: %s %s", sys.cmd, collapse(sys.args, " "))
    res = try(system3(sys.cmd, sys.args, stdin = stdin, stdout = TRUE, stderr = TRUE, wait = TRUE, stop.on.exit.code = stop.on.exit.code))
    catf("OS result:")
    print(res)
  } else {
    res = system3(sys.cmd, sys.args, stdin = stdin, stdout = TRUE, stderr = TRUE, wait = TRUE, stop.on.exit.code = stop.on.exit.code)
  }
  if(is.error(res))
    stopf("Error in runLinuxOSCommand: %s (cmd: %s || args: %s)", as.character(res), sys.cmd, collapse(sys.args))
  res
}

# Find helper script on a Linux machine in package dir.
# @param rhome [\code{character(1)}]
#   RHOME dir.
# @param r.options [\code{character}]
#   Options for R and Rscript, one option per element of the vector,
#   a la "--vanilla".
# @param ssh [\code{logical(1)}]
#   Use SSH?
#   Default is \code{FALSE}.
# @param nodename [\code{character(1)}]
#   Nodename for SSH.
# @return [\code{character(1)}]. Path of script.
findHelperScriptLinux = function(rhome, r.options, ssh = FALSE, nodename) {
  # i think we dont need to quote anything here, because system2 uses shQuote
  if (rhome == "")
    rscript = "Rscript"
  else
    rscript = file.path(rhome, "bin", "Rscript")
  minus.e = "-e \"message(normalizePath(system.file(\\\"bin/linux-helper\\\", package = \\\"BatchJobs\\\")))\""
  args = c(r.options, minus.e)
  # only take the last line, if stuff is printed by Rscript before our message
  tail(runOSCommandLinux(rscript, args, ssh = ssh, nodename = nodename)$output, 1L)
}

# Perform a batch helper command on a Linux machine.
# @param worker [\code{\link{WorkerLinux}}]
#   Worker.
# @param command [\code{character(1)}]
#   Helper command.
# @param args [\code{character}]
#   Arguments for helper command.
# See documenation of linux-helper.
runWorkerCommand = function(worker, command, args = character(0L)) {
  # in paths can be whitespaces and other bad stuff, quote it!
  args = sprintf("\"%s\"", args)
  script.args = c(command, args)
  runOSCommandLinux(worker$script, script.args, ssh = worker$ssh, nodename = worker$nodename)$output
}

