# FIXME: document these as they might be relevant for users
cfReadBrewTemplate = function(template.file) {
  checkArg(template.file, "character", len=1L, na.ok=FALSE)
  fd = file(template.file, "r")
  template = paste(readLines(fd), collapse="\n")
  close(fd)
  return(template)
}

cfBrewTemplate = function(conf, template, rscript) {
  if (conf$debug) {
    # if not temp, use jobs dir
    outfile = sub("\\.R$", ".pbs", rscript)
  } else {
    outfile = tempfile()
  }
  pf = parent.frame()
  old = getOption("show.error.messages")
  options(show.error.messages=FALSE)
  z = suppressAll(brew(text=template,  output=outfile, envir=pf))
  options(show.error.messages=old)
  if (is.error(z))
    stop(z)
  return(outfile)
}

cfHandleUnkownSubmitError = function(cmd, res) {
  msg = sprintf("%s produced exit code %i; output %s", 
    cmd, res$exit.code, collapse(res$output, sep="\n"))
  makeSubmitJobResult(status=101L, batch.job.id=NA_character_, msg=msg)
}

cfKillBatchJob = function(cmd, batch.job.id, max.tries=3L) { 
  tries = 0L
  while(TRUE) {
    # qdel sends SIGTERM, delay, SIGKILL
    res = runOSCommandLinux(cmd, batch.job.id, stop.on.exit.code=FALSE)
    if (res$exit.code == 0L) {
      return()
    } else {
      tries = tries + 1L
      if (tries > max.tries) {
        stopf("Really tried to kill job, but could not do it. batch job id is %s.\nMessage: %s",
          batch.job.id, collapse(res$output, sep="\n"))
      }
      Sys.sleep(1)
    }
  }
}
