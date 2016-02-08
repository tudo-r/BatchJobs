getJobInfoInternal = function(reg, ids, select, unit = "seconds", columns) {
  if (!missing(ids))
    ids = checkIds(reg, ids)
  assertChoice(unit, c("seconds", "minutes", "hours", "days", "weeks"))

  select.db   = c("submitted",      "started",      "done",       "CASE WHEN error IS NULL THEN done - started ELSE error_time - started END AS time_running", "memory", "started - submitted AS time_queued", "error",      "error_time",     "node",     "batch_job_id", "r_pid", "seed")
  select.cns  = c("time.submitted", "time.started", "time.done",  "time.running",                                                                              "memory", "time.queued",                        "error.msg",  "time.error",     "nodename", "batch.id",     "r.pid", "seed")
  columns = c(columns, setNames(select.db, select.cns))

  if (!missing(select)) {
    assertSubset(select, c("id", select.cns))
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
  if (!is.null(tab$time.error))
    tab$time.error = dbConvertNumericToPOSIXct(tab$time.error)

  # shorten error messages
  if (!is.null(tab$error.msg))
    tab$error.msg = vcapply(tab$error.msg, clipString, len = 30L)

  # convert time diffs
  div = setNames(c(1L, 60L, 3600L, 86400L, 604800L), c("seconds", "minutes", "hours", "days", "weeks"))[unit]
  if (!is.null(tab$time.running))
    tab$time.running = as.numeric(tab$time.running) / div
  if (!is.null(tab$time.queued))
    tab$time.queued = as.numeric(tab$time.queued) / div

  return(tab)
}

#' @title Get computational information of jobs.
#'
#' @description
#' Returns time stamps (submitted, started, done, error),
#' time running, approximate memory usage (in Mb),
#' error messages (shortened, see \code{\link{showLog}} for detailed error messages),
#' time in queue, hostname of the host the job was executed,
#' assigned batch ID, the R PID and the seed of the job.
#'
#' To estimate memory usage the sum of the last column of \code{\link[base]{gc}} is used.
#'
#' Column \dQuote{time.running} displays the time until either the job was done, or an error occured;
#' it will by \code{NA} in case of time outs or hard R crashes.
#'
#' @template arg_reg
#' @template arg_ids
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
#' @family debug
#' @export
getJobInfo = function(reg, ids, pars = FALSE, prefix.pars = FALSE, select, unit = "seconds") {
  UseMethod("getJobInfo")
}

#' @method getJobInfo Registry
#' @export
getJobInfo.Registry = function(reg, ids, pars = FALSE, prefix.pars = FALSE, select, unit = "seconds") {
  checkRegistry(reg, writeable = FALSE)
  syncRegistry(reg)
  assertFlag(pars)
  columns = c(id = "job_id")
  if (pars)
    columns = c(columns, c(pars = "pars"))

  tab = getJobInfoInternal(reg, ids = ids, select = select, unit = unit, columns = columns)

  if (pars && nrow(tab) > 0L && !is.null(tab$pars)) {
    pars = deserialize(tab$pars)
    if (prefix.pars)
      setnames(pars, sprintf("job.par.%s", names(pars)))
    tab = cbind(dropNamed(tab, "pars"), pars)
  }

  return(tab)
}
