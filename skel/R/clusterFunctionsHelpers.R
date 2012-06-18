# FIXME: document these as they might be relevant for users
readBrewTemplate = function(template.file) {
  checkArg(template.file, "character", len=1L, na.ok=FALSE)
  fd = file(template.file, "r")
  template = paste(readLines(fd), collapse="\n")
  close(fd)
  return(template)
}

brewWithStop = function(conf, template, rscript) {
  if (conf$debug) {
    # if not temp, use jobs dir
    outfile = sub("\\.R$", ".pbs", rscript)
  } else {
    outfile = tempfile()
  }
  pf = parent.frame()
  old = getOption("show.error.messages")
  options(show.error.messages=FALSE)
  z = suppressAll(brew(..., envir=pf))
  options(show.error.messages=old)
  if (is.error(z))
    stop(z)
  return(outfile)
}

killBatchJob = function(cmd, batch.job.id, max.tries=3L) { 
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
          batch.job.id, paste(res$output, "\n"))
      }
      Sys.sleep(1)
    }
  }
}
