# Runs a command, either by SSH or directly on localhost.
#
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
runOSCommandLinux = function(cmd, args=character(0L), stdin="", ssh=FALSE, nodename, max.retries, wait) {
  #FIXME
  #conf = getBatchJobsConf()
  
  temperror.exitcodes = conf$temperror.exitcodes
  temperror.messages = conf$temperror.messages
  max.retries = conf$temperror.retries
  wait = conf$temperror.wait
  
  # is ec1/msg1 a match to one of the predefinedtemp error?
  isTempErrorMatch = function(ec1, msg1, ec2, msg2) {
    (is.na(ec2) || ec1 == ec2) && (is.na(msg2) || length(grep(msg2, msg1) > 0L))
  }
  
  # do we match any of the specified temp errors by the user?
  isTempError = function(exitcode, msg) {
    any(mapply(isTempErrorMatch, temperror.exitcodes, temperror.messages, 
      MoreArgs=list(ec1=exitcode, msg1=msg)))
  }
  
  runMultipleTimes = function(cmd, args) {
    tries = 1L
    while(TRUE) {
      res = system3(cmd, args, stdin=stdin, stdout=TRUE, stderr=TRUE, wait=TRUE, stop.on.exit.code=FALSE)
      temperror = isTempError(res$exit.code, res$output)
      print(temperror)
      # we encountered a fatal, unknown error. stop with exception
      if (!temperror && res$exit.code > 0L)
        stopf("Error in runLinuxOSCommand: exitcode = %i\n%s\ncmd: %s; args: %s",
          res$exit.code, res$output, cmd, collapse(args))
      # no temp error and exit code ok, we can exit, success
      if (!temperror && res$exit.code == 0L)
        return(res)
      # we have tried multiple times, but always temp error
      if (tries > max.retries)
        stopf("Error in runLinuxOSCommand: Tried %i times, but always encountered temp errors.", tries)
      wait(tries)
      tries = tries + 1L
    }
  }
  
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
    res = runMultipleTimes(sys.cmd, sys.args)
    catf("OS result:")
    print(res)
  } else {
    res = runMultipleTimes(sys.cmd, sys.args)
  }
  return(res)
}


conf = list(
  debug = TRUE,
  temperror.retries = 2,
  temperror.wait = function(i) 1,
  temperror.exitcodes = 2,
  temperror.messages = NA
)

runOSCommandLinux(cmd="ls", args=".")
