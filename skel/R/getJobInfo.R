#' Get job informations.
#'
#' Returns time stamps (submitted, started, done), time running, time in queue,
#' error messages (shortened, see \code{\link{showLog}} for detailed error messages),
#' hostname of the host the job was executed, the assigned batch ID, the R PID and the seed of the job.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param unit [\code{character}]\cr
#'   Unit to convert execution and queing times to.
#'   Possible values: \dQuote{seconds}, \dQuote{minutes}, \dQuote{hours},
#'   \dQuote{days} and \dQuote{weeks}.
#'   Default is \dQuote{seconds}.
#' @return [\code{data.frame}].
#' @export
getJobInfo = function(reg, ids, unit = "seconds") {
  # FIXME we need to set this generic for BatchExperiments
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  units = c("seconds", "minutes", "hours", "days", "weeks")
  checkArg(unit, units)

  columns = c("job_id", "submitted", "started", "done", "done - started AS time_running", "submitted - started AS time_queued", "error", "node", "batch_job_id", "r_pid", "seed")
  pretty = c("job.id", "time.submitted", "time.started", "time.done", "time.running", "time.queued", "error.msg", "nodename", "batch.id", "r.pid", "seed")

  tab = dbGetExpandedJobsTable(reg, ids, columns)

  # convert times to POSIX
  tab$submitted = dbConvertNumericToPOSIXct(tab$submitted)
  tab$started = dbConvertNumericToPOSIXct(tab$started)
  tab$done = dbConvertNumericToPOSIXct(tab$done)

  # shorten error messages
  tab$error = vapply(tab$error, shortenString, "", len = 30L)

  # convert time diffs
  div = setNames(c(1L, 60L, 3600L, 86400L, 604800L), units)[unit]
  tab$time_running = prettyNum(tab$time_running / div)
  tab$time_queued = prettyNum(tab$time_queued / div)

  setNames(tab, pretty)
}

