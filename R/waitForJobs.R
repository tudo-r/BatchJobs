#' Wait for termination of jobs on the batch system.
#'
#' @description
#' Waits for termination of jobs while displaying a progress bar
#' containing summarizing informations of the jobs.
#' The following abbreviations are used in the progress bar:
#' \dQuote{S} for number of jobs on system, \dQuote{D} for number of
#' jobs successfully terminated, \dQuote{E} for number ofjobs terminated
#' with an R exception and \dQuote{R} for number of jobs currently running
#' on the system.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Vector of job ids.
#'   Default is all submitted jobs not yet terminated.
#' @param sleep [\code{numeric(1)}]\cr
#'   Seconds to sleep between status updates. Default is \code{10}.
#' @param timeout [\code{numeric(1)}]\cr
#'   After waiting \code{timeout} seconds, show a message and return \code{FALSE}.
#'   This argument may be required on some systems where, e.g., expired jobs or jobs on hold
#'   are problematic to detect. If you don't want a timeout, set this to \code{Inf}.
#'   Default is \code{604800} (one week).
#' @param stop.on.error [\code{logical(1)}]\cr
#'   Immediately return if a job terminates with an error? Default is \code{FALSE}.
#' @template arg_progressbar
#' @return [\code{logical(1)}]. Returns \code{TRUE} if all jobs terminated successfully
#'   and \code{FALSE} if either an error occurred or the timeout is reached.
#' @export
waitForJobs = function(reg, ids, sleep = 10, timeout = 604800, stop.on.error = FALSE,
  progressbar = TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindSubmittedNotTerminated(reg)
  } else {
    ids = checkIds(reg, ids)
    not.submitted = dbFindSubmitted(reg, ids, negate = TRUE, limit = 1L)
    if (length(not.submitted) > 0L)
      stopf("Not all jobs have been submitted, e.g. job with id %i", not.submitted)
  }
  assertNumber(sleep, lower = 1)
  if (is.infinite(sleep))
    stop("Argument 'sleep' must be finite")
  assertNumber(timeout, lower = sleep)
  assertFlag(stop.on.error)
  assertFlag(progressbar)

  n = length(ids)
  if (n == 0L)
    return(TRUE)
  timeout = now() + timeout
  batch.ids = getBatchIds(reg, "Cannot find jobs on system")
  i = 1L

  bar = getProgressBar(progressbar, min = 0L, max = n, label = "Waiting                  ")
  on.exit(bar$kill())

  repeat {

    stats = dbGetStats(reg, ids, running = TRUE, expired = FALSE, times = FALSE, batch.ids = batch.ids)
    n.sys = n - stats$done - stats$error
    bar$set(n - n.sys, msg = sprintf("Waiting [S:%i D:%i E:%i R:%i]", n.sys, stats$done, stats$error, stats$running))

    if (stop.on.error && stats$error) {
      err = dbGetErrorMsgs(reg, ids, filter = TRUE, limit = 1L)
      warningf("Job %i terminated with an error: %s", err$job_id, err$error)
      return(FALSE)
    }

    if (n.sys == 0L)
      return(stats$error == 0L)

    if (i %% 5L == 0L) {
      # update batch ids
      batch.ids = getBatchIds(reg, "Cannot find jobs on system")

      # check if there are still jobs on the system and none has mystically disappeared
      # NOTE it seems like some schedulers are "laggy", we should not do this operation
      # in the first loop w/o a sleep
      if (length(dbFindOnSystem(reg, ids, limit = 1L, batch.ids = batch.ids)) == 0L) {
        if (length(dbFindDisappeared(reg, ids, limit = 1L, batch.ids = batch.ids)) > 0L)
          bar$error(stop("Some jobs disappeared, i.e. were submitted but are now gone. Check your configuration and template file."))
        return(stats$error == 0L)
      }
    }

    if (is.finite(timeout) && now() > timeout) {
      warningf("Timeout reached. %i jobs still on system.", n.sys)
      return(FALSE)
    }

    Sys.sleep(sleep)
    i = i + 1L
    suppressMessages(syncRegistry(reg))
  }
}
