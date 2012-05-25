# ******************** Constructors ********************

# Construct a remote worker for a Linux machine via SSH.
makeWorkerRemoteLinux = function(nodename, rhome, script, ncpus, max.jobs, max.load) {
  makeWorker(ssh=TRUE, nodename, rhome, script, ncpus, max.jobs, max.load, c("WorkerRemoteLinux", "WorkerLinux"))
}

# Construct a worker for local Linux machine to spawn parallel jobs.
makeWorkerLocalLinux = function(script, ncpus, max.jobs, max.load) {
  makeWorker(ssh=FALSE, "localhost", R.home(), script, ncpus, max.jobs, max.load, c("WorkerLocalLinux", "WorkerLinux"))
}

# ******************** Interface implementation ********************

#' @S3method getWorkerNumberOfCPUs WorkerLinux
getWorkerNumberOfCPUs.WorkerLinux = function(worker) {
  as.integer(runWorkerCommand(worker, "number-of-cpus"))
}

#' @S3method getWorkerStatus WorkerLinux
getWorkerStatus.WorkerLinux = function(worker, file.dir) {
  res = runWorkerCommand(worker, "status", file.dir)
  res = as.list(as.numeric(strsplit(res, " +")[[1L]]))
  names(res) = c("load", "n.rprocs", "n.rprocs.50", "n.jobs")
  return(res)
}

#' @S3method startWorkerJob WorkerLinux
startWorkerJob.WorkerLinux = function(worker, rfile, outfile) {
  runWorkerCommand(worker, "start-job", c(worker$rhome, rfile, outfile))
}

#' @S3method killWorkerJob WorkerLinux
killWorkerJob.WorkerLinux = function(worker, pid) {
  runWorkerCommand(worker, "kill-job", pid)
}

#' @S3method listWorkerJobs WorkerLinux
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
# @param stop.on.exit.code [\code{character}]
#   See \code{\link{system3}}.
#   Default is \code{TRUE}.
# @param ssh [\code{logical(1)}]
#   Use SSH?
#   Default is \code{FALSE}.
# @param nodename [\code{character(1)}]
#   Nodename for SSH.
# @return See \code{\link{system3}}.
runOSCommandLinux = function(cmd, args=character(0L), stop.on.exit.code=TRUE, ssh=FALSE, nodename) {
  conf = getBatchJobsConf()
  if (ssh) {
    sys.cmd = "ssh"
    ssh.cmd = sprintf("'%s'", collapse(c(cmd, args), sep=" "))
    sys.args = c(nodename, ssh.cmd)
  } else {
    sys.cmd = cmd
    sys.args = args
  }
  if (conf$debug) {
    catf("OS cmd: %s %s", sys.cmd, collapse(sys.args, " "))
    res = try(system3(sys.cmd, sys.args, stdout=TRUE, stderr=TRUE, wait=TRUE, stop.on.exit.code=stop.on.exit.code))
  } else {
    res = system3(sys.cmd, sys.args, stdout=TRUE, stderr=TRUE, wait=TRUE, stop.on.exit.code=stop.on.exit.code)
  }
  if (conf$debug) {
    catf("OS result:")
    print(res)
  }
  if(is.error(res))
    stopf("Error in runLinuxOSCommand: %s (cmd: %s || args: %s)", as.character(res), sys.cmd, collapse(sys.args))
  res
}

# Find helper script on a Linux machine in package dir.
# @param rhome [\code{character(1)}]
#   RHOME dir.
# @param ssh [\code{logical(1)}]
#   Use SSH?
#   Default is \code{FALSE}.
# @param nodename [\code{character(1)}]
#   Nodename for SSH.
# @return [\code{character(1)}]. Path of script.
findHelperScriptLinux = function(rhome, ssh=FALSE, nodename) {
  # i think we dont need to quote anything here, because system2 uses shQuote
  if (rhome == "")
    rscript = "Rscript"
  else
    rscript = file.path(rhome, "bin", "Rscript")
  minus.e = "-e \"message(normalizePath(system.file(\\\"bin/linux-helper\\\", package=\\\"BatchJobs\\\")))\""
  runOSCommandLinux(rscript, minus.e, ssh=ssh, nodename=nodename)$output
}

# Perform a batch helper command on a Linux machine.
# @param worker [\code{\link{WorkerLinux}}]
#   Worker.
# @param command [\code{character(1)}]
#   Helper command.
# @param args [\code{character}]
#   Arguments for helper command.
# See documenation of linux-helper.
runWorkerCommand = function(worker, command, args=character(0L)) {
  # in paths can be whitespaces and other bad stuff, quote it!
  args = sprintf("\"%s\"", args)
  script.args = c(command, args)
  runOSCommandLinux(worker$script, script.args, ssh=worker$ssh, nodename=worker$nodename)$output
}

