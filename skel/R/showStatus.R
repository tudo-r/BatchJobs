#' Show status information about jobs.
#' E.g.: How many there are, how many are done, any errors, etc.
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
#' @return [\code{data.frame}] Data frame of job status from database of registry. Returned invisbly.
#' @examples \dontrun{
#'  reg <- makeRegistry(id="BatchJobsExample", seed=123)
#'  f <- function(x) x^2
#'  batchMap(reg, f, 1:10)
#'  submitJobs(reg)
#'
#' # after a few moments:
#' showStatus(reg)
#' # should show 10 submitted jobs, which are all done.
#' }
#' @export
showStatus = function(reg, ids, run.and.exp=TRUE, errors = 10L) {
  checkArg(reg, "Registry")
  if (missing(ids)) {
    df = dbGetJobStatusTable(reg)
    ids = df$job_id
  } else {
    if (length(ids) == 0L) {
      message("Empty ids vector!")
      return(invisible(df[integer(0L),,drop=FALSE]))
    }
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
    df = dbGetJobStatusTable(reg, ids)
  }
  if (nrow(df) == 0L)  {
    message("No (matched) jobs in registry!")
    return(invisible(df))
  }
  getPerc = function(x) paste(round(mean(x)*100, 2L), "%", sep="")
  submitted = !is.na(df$submitted)
  started = !is.na(df$started)
  dones = !is.na(df$done)
  errs = !is.na(df$error)
  # convert to numeric so we always get the difference in seconds
  times = na.omit(as.integer(df$done) - as.integer(df$started))
  if (length(times) == 0L)
    times = as.integer(NA)
  n.errs = sum(errs)
  catf("Status for jobs: %i", nrow(df))
  catf("Submitted: %i (%s)", sum(submitted), getPerc(submitted))
  catf("Started: %i (%s)", sum(started), getPerc(started))
  if(run.and.exp && !is.null(getListJobs())) {
    running = ids %in% findRunning(reg)
    expired = ids %in% findExpired(reg)
    catf("Running: %i (%s)", sum(running), getPerc(running))
    catf("Expired: %i (%s)", sum(expired), getPerc(expired))
  }
  catf("Errors: %i (%s)",  n.errs, getPerc(errs))
  catf("Done: %i (%s)",  sum(dones), getPerc(dones))
  catf("Time: min=%.02f med=%.02f max=%.02f", min(times), median(times), max(times))

  m = min(errors, n.errs)
  if (m > 0L) {
    catf("\nShowing first %i errors: ", m)
    err.inds = which(errs)
    for (i in seq_len(m))
      catf("Error in %s: %s", ids[err.inds[i]], df$error[err.inds[i]])
  }
  invisible(df)
}

