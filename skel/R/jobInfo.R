jobInfo = function(reg, ids, unit = "seconds") {
  shorten = function(x, len) {
    if (is.na(x))
      return(NA_character_)
    if (nchar(x) > len)
      return(paste(substr(x, 1L, len - 3L), "...", sep=""))
    x
  }

  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)

  columns = c("job_id", "submitted", "started", "done", "done - started AS time_running", "submitted - started AS time_queued", "error", "node", "batch_job_id", "r_pid", "seed")
  pretty = c("job.id", "time.submitted", "time.started", "time.done", "time.running", "time.queued", "error.msg", "nodename", "batch.id", "r.pid", "seed")

  tab = dbGetExpandedJobsTable(reg, ids, columns)

  # convert times to POSIX
  tab$submitted = dbConvertNumericToPOSIXct(tab$submitted)
  tab$started = dbConvertNumericToPOSIXct(tab$started)
  tab$done = dbConvertNumericToPOSIXct(tab$done)

  # shorten error messages
  tab$error = vapply(tab$error, shorten, "", len = 30L)

  # convert time diffs
  tab$time_running = prettyNum(timeconv(tab$time_running, unit))
  tab$time_queued = prettyNum(timeconv(tab$time_queued, unit))

  setNames(tab, pretty)
}
