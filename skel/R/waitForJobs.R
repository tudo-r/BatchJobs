#' Wait for termination of jobs on the batch system.
#'
#' Waits for termination of jobs while displaying a progress bar
#' containing summarizing informations of the jobs.
#' The following abbrevations are used in the progress bar:
#' \dQuote{S} for number of jobs on system, \dQuote{R} for number of jobs running
#' \dQuote{D} for number of jobs successfully terminated and \dQuote{E} for number of
#' jobs terminated with an R exception.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{\link{matrix}}]\cr
#'   Vector of job ids.
#'   Default is all jobs currently on the system.
#' @param sleep [\code{numeric(1)}]\cr
#'   Seconds to sleep between updates. Default is \code{10}.
#' @param timeout [\code{numeric(1)}]\cr
#'   After waiting \code{timeout} seconds, show a message and return \code{FALSE}.
#'   This argument may be required on some systems where, e.g., expired jobs or jobs on hold
#'   are problematic to detect. If you don't want a timeout, set this to \code{Inf}.
#'   Default is \code{604800} (one week).
#' @param stop.on.error [\code{logical(1)}]\cr
#'   Stop if a job terminates with an error? Default is \code{FALSE}.
#' @return Returns \code{TRUE} on success and \code{FALSE} if either
#'   \code{stop.on.error} is \code{TRUE} and an error occured or the timeout is reached.
#' @export
waitForJobs = function(reg, ids, sleep = 10, timeout = 604800, stop.on.error = FALSE) {
  checkRegistry(reg)
  syncRegistry(reg)

  batch.ids = getBatchIds(reg, "Cannot find jobs on system")

  if (missing(ids))
    ids = dbFindOnSystem(reg, ids, batch.ids = batch.ids)
  else
    ids = checkIds(reg, ids)

  checkArg(sleep, "numeric", len=1L, lower=1, na.ok=FALSE)
  if (is.infinite(sleep))
    stop("Argument 'sleep' must be finite")
  checkArg(timeout, "numeric", len=1L, lower=sleep, na.ok=FALSE)
  checkArg(stop.on.error, "logical", len=1L, na.ok=FALSE)

  n = length(ids)
  if (n > 0L) {
    timeout = now() + timeout
    bar = makeProgressBar(min=0L, max=n, label="Waiting                  ")

    repeat {
      # FIXME overhead in query
      on.sys = length(dbFindOnSystem(reg, ids, batch.ids = batch.ids))
      stats = dbGetStats(reg, ids, running=TRUE, expired=FALSE, times=FALSE, batch.ids = batch.ids)
      bar$set(n - on.sys, msg = sprintf("Waiting [S:%i R:%i D:%i E:%i]", on.sys, stats$running, stats$done, stats$error))

      if (stop.on.error && stats$error > 0L) {
        err = dbGetErrorMsgs(reg, ids, filter=TRUE, limit=1L)
        messagef("Job $i terminated with an error: %s", err$job_id, err$error)
        return(FALSE)
      }

      if (on.sys == 0L || is.finite(timeout) && now() > timeout)
        break

      Sys.sleep(sleep)
      suppressMessages(syncRegistry(reg))
      batch.ids = getBatchIds(reg, "Cannot find jobs on system")
    }

    bar$kill()

    if (on.sys > 0L) {
      messagef("Timeout reached. %i jobs still on system.", on.sys)
      return(FALSE)
    }
  }
  message("All jobs terminated.")
  return(TRUE)
}
