# Runs a command, either by SSH or directly on localhost.
# @param cmd [\code{character(1)}]
#   System command to run. 
# @param args [\code{character}]
#   System command arguments. 
#   Default is \code{character(0)}.
# @param ssh [\code{logical(1)}]
#   Use SSH?
#   Default is \code{FALSE}.
# @param nodename [\code{character(1)}]
#   Nodename for SSH.
# @return [\code{character(1)}]. Result of command.
runCommand = function(cmd, args=character(0L), ssh=FALSE, nodename) {
  if (ssh) {
    sys.cmd = "ssh"
    ssh.cmd = sprintf("'%s'", collapse(c(cmd, args), sep=" "))
    sys.args = c(nodename, ssh.cmd)
  } else {
    sys.cmd = cmd
    sys.args = args
  }
  res = try(system2(sys.cmd, sys.args, stdout=TRUE, stderr=TRUE))
  if(is.error(res))
    stopf("Error in runCommand: %s (cmd: %s || args: %s)", as.character(res), sys.cmd, collapse(sys.args))
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
  minus.e = "-e \"message(system.file(\\\"bin/linux-helper\\\", package=\\\"BatchJobs\\\"))\""
  runCommand(rscript, minus.e, ssh, nodename)
}

# Perform a batch helper command on a Linux machine.
# @param worker [\code{\link{WorkerLinux}}] 
#   Worker.
# @param command [\code{character(1)}]
#   number-of-cpus, worker-status, start-job, kill-job, running-jobs
# @param args [\code{character}] 
#   Arguments for helper command.
# See documenation of linux-helper.
onWorkerLinux = function(worker, command, args=character(0L)) {
  script.args = c(worker$rhome, command, args)
  # in paths can be whitespaces and other bad stuff, quote it!
  script.args = sprintf("\"%s\"", script.args)
  runCommand(worker$script, script.args, worker$ssh, worker$nodename))
}

