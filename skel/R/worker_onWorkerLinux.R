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
runCommand = function(cmd, args=character(0), ssh=FALSE, nodename) {
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
    stopf("Error in runCommand: %s (res: %s || args: %s)", as.character(res), sys.cmd, collapse(sys.args))
  evaluated
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
  rscript = sprintf("%s/bin/Rscript", rhome)
  minus.e = "-e \"message(system.file(\\\"bin/ssh-helper\\\", package=\\\"BatchJobs\\\"))\""
  runCommand(rscript, minus.e, ssh, nodename)
}

# Perform a batch helper command on a Linux machine.
# @param worker [\code{\link{WorkerLinux}}] 
#   Worker.
# @param command [\code{character(1)}]
#   number-of-processors, worker-status, start-job, kill-job, running-jobs
# @param args [\code{character}] 
#   Arguments for helper command.
#   number-of-processors: Nothing.
#   worker-status: job.dir
#   start-job: work.dir, rfile, outfile.
#   kill-job: pid.
#   running-jobs: Nothing.
# @return [any]. 
#   number-of-processors [\code{integer(1)}]: Number of CPUs.
#   worker-status [\code{numeric(3)}]: Load, number of running R jobs, number of expensive R jobs.
#   start-job [\code{character(1)}]: PID.
#   kill-job [any]: Nothing
#   running-jobs [\code{character(1)}]: PIDs of running jobs for this registry.
onWorkerLinux = function(worker, command, args=character(0)) {
  script.args = c(worker$rhome, command, args)
  # in paths can be whitespaces and other bad stuff, quote it!
  script.args = sprintf("\"%s\"", script.args)
  res = runCommand(worker$script, script.args, worker$ssh, worker$nodename)
  tryCatch(eval(parse(text=paste(res, collapse="\n"))),
    error=function(x) stop(res))
}


