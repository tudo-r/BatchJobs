#' @title Retrieve or show status information about jobs.
#'
#' @description
#' E.g.: How many there are, how many are done, any errors, etc.
#' \code{showStatus} displays on the console, \code{getStatus} returns an informative result
#' without console output.
#
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected jobs.
#'   Default is all jobs.
#' @param errors [\code{integer(1)}]\cr
#'   How many of the error messages should be displayed if any errors occured in the jobs?
#'   Default is 10.
#' @param run.and.exp [\code{logical(1)}]\cr
#'   Show running and expired jobs?
#'   Requires to list the job on the batch system. If not possible, because
#'   that cluster function is not avilable, this option is ignored anyway.
#'   Default is \code{TRUE}.
#' @return [\code{list}]. List of absolute job numbers. \code{showStatus} returns them
#'   invisibly.
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # should show 10 submitted jobs, which are all done.
#' showStatus(reg)
showStatus = function(reg, ids, run.and.exp = TRUE, errors = 10L) {
  errors = asCount(errors)

  stats = getStatus(reg, ids = ids, run.and.exp = run.and.exp)

  procent = function(x, n) {
    if(is.na(x))
      return("")
    p = ifelse(n == 0L, 0, x / n * 100)
    sprintf("(%6.2f%%)", p)
  }

  output = collapse(c("Status for %%i jobs at %%s",
                      "Submitted: %%%1$ii %%s",
                      "Started:   %%%1$ii %%s",
                      "Running:   %%%1$ii %%s",
                      "Done:      %%%1$ii %%s",
                      "Errors:    %%%1$ii %%s",
                      "Expired:   %%%1$ii %%s",
                      "Time: min=%%.2fs avg=%%.2fs max=%%.2fs"), "\n")

  output = sprintf(output, min(4L, nchar(sprintf("%i", stats$n + 1L))))
  with(stats, catf(output, n, Sys.time(), submitted, procent(submitted, n), started, procent(started, n),
                   running, procent(running, n), done, procent(done, n), error, procent(error, n),
                   expired, procent(expired, n), t_min, t_avg, t_max))

  m = min(errors, stats$error)
  if(m > 0L) {
    msgs = dbGetErrorMsgs(reg, ids, filter = TRUE, limit = m)
    catf("\nShowing first %i errors:", m)
    cat(sprintf("Error in %i: %s", msgs$job_id, msgs$error), sep = "\n")
  }

  return(invisible(stats))
}


#' @rdname showStatus
#' @export getStatus
getStatus = function(reg, ids, run.and.exp = TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)

  run.and.exp = run.and.exp && !is.null(getListJobs())
  dbGetStats(reg, ids, running = run.and.exp, expired = run.and.exp, times = TRUE)
}
