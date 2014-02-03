getJobInfoInternal = function(reg, ids, pars, select, unit, columns) {
  if (!missing(ids))
    ids = checkIds(reg, ids)
  checkArg(unit, choices=c("seconds", "minutes", "hours", "days", "weeks"))

  select.db   = c("submitted",      "started",      "done",       "done - started AS time_running", "started - submitted AS time_queued", "error",      "node",     "batch_job_id", "r_pid", "seed")
  select.cns  = c("time.submitted", "time.started", "time.done",  "time.running",                  "time.queued",                        "error.msg",   "nodename", "batch.id",     "r.pid", "seed")
  columns = c(columns, setNames(select.db, select.cns))

  if (!missing(select)) {
    checkArg(select, subset=c("id", select.cns))
    columns = columns[names(columns) %in% c("id", select)]
  }

  tab = setNames(dbGetExpandedJobsTable(reg, ids, columns), names(columns))

  if (nrow(tab) == 0L)
    return(tab)

  # convert times to POSIX
  if (!is.null(tab$time.submitted))
    tab$time.submitted = dbConvertNumericToPOSIXct(tab$time.submitted)
  if (!is.null(tab$time.started))
    tab$time.started = dbConvertNumericToPOSIXct(tab$time.started)
  if (!is.null(tab$time.done))
    tab$time.done = dbConvertNumericToPOSIXct(tab$time.done)

  # shorten error messages
  if (!is.null(tab$error.msg))
    tab$error.msg = vapply(tab$error.msg, clipString, "", len = 30L)

  # convert time diffs
  div = setNames(c(1L, 60L, 3600L, 86400L, 604800L), c("seconds", "minutes", "hours", "days", "weeks"))[unit]
  if (!is.null(tab$time.running))
    tab$time.running = as.numeric(tab$time.running) / div
  if (!is.null(tab$time.queued))
    tab$time.queued = as.numeric(tab$time.queued) / div

  return(tab)
}

#' Get computational information of jobs.
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
#' @param pars [\code{logical(1)}]\cr
#'   Include job parameters in the output?
#'   Default is \code{FALSE}.
#' @param prefix.pars [\code{logical(1)}]\cr
#'   Should a prefix be added to job parameter names (column names) to avoid name clashes?
#'   Default is \code{FALSE}.
#' @param select [\code{character}]\cr
#'   Select only a subset of columns.
#'   Usually this is not required and you can subset yourself,
#'   but in some rare cases it may be advantageous to not query all information.
#'   Note that the column \dQuote{id} (job id) is always selected.
#'   If not provided, all columns are queried and returned.
#' @param unit [\code{character(1)}]\cr
#'   Unit to convert execution and queing times to.
#'   Possible values: \dQuote{seconds}, \dQuote{minutes}, \dQuote{hours},
#'   \dQuote{days} and \dQuote{weeks}.
#'   Default is \dQuote{seconds}.
#' @return [\code{data.frame}].
#' @export
getJobInfo = function(reg, ids, pars=FALSE, prefix.pars=FALSE, select, unit="seconds") {
  UseMethod("getJobInfo")
}

#' @method getJobInfo Registry
#' @S3method getJobInfo Registry
getJobInfo.Registry = function(reg, ids, pars=FALSE, prefix.pars=FALSE, select, unit="seconds") {
  syncRegistry(reg)
  checkArg(pars, "logical", len=1L, na.ok=FALSE)
  columns = c(id="job_id")
  if (pars)
    columns = c(columns, c(pars="pars"))

  tab = getJobInfoInternal(reg, ids, pars, select, unit, columns)

  # unserialize parameters
  if (pars && !is.null(tab$pars)) {
    pars = list2df(lapply(tab$pars, function(x) unserialize(charToRaw(x))), force.names=TRUE)
    if (prefix.pars)
      names(pars) = sprintf("job.par.%s", names(pars))
    tab = cbind(subset(tab, select=-pars), pars)
  }
  return(tab)
}
