#' Kill a job on the batch system.
#'
#' Kill jobs which have already been submitted to the batch system.
#'
#' If a job is killed its internal state is reset as if it had not been submitted at all.
#' The function warns if
#' (a) the job you want to kill has not been submitted,
#' (b) the job is already done,
#' (c) the job already terminated with an error.
#' In all 3 cases above nothing is changed for the state of this job and no call
#' to the internal kill cluster function is generated.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @return Nothing.
#' @export
killJobs = function(reg, ids) {
  checkArg(reg, cl="Registry")
  if (missing(ids))
    return(invisible(NULL))
  else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
  }
  killfun = getKillJob("Cannot kill jobs")
  data = dbGetJobStatusTable(reg, ids)
  messagef("Trying to kill %i jobs.", length(ids))
  messagef("For safety reasons waiting 3secs...")
  Sys.sleep(3)
  n.unsubm = sum(is.na(data$submitted))
  n.term = sum(!is.na(data$done) | !is.na(data$error))
  messagef("Not submitted: %i", n.unsubm)
  messagef("Already terminated: %i", n.term)
  messagef("No batch.job.id: %i", n.term)
  # must submitted, be not done, no error, has bji
  data = subset(data, !is.na(data$submitted) & is.na(data$done)
    & is.na(data$error) & !is.na(data$batch_job_id))
  # unique because of chunking
  ids.batch = unique(data$batch_job_id)
  ids.job = data$job_id
  messagef("Killing batch.job.ids: %i", length(ids.batch))
  if (length(ids.batch) > 0L) {
    ids.str = collapse(ids.batch)
    # trim string so its not too long
    if (str_length(ids.str) > 200L)
      ids.str = sprintf("%s...", str_sub(ids.str, end=200L))
    message(ids.str)
    conf = getBatchJobsConf()
    lapply(ids.batch, killfun, reg=reg, conf=conf)
  }
  messagef("For safety reasons waiting 3secs...")
  Sys.sleep(3)
  messagef("Resetting %i jobs in DB.", length(ids.job))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids.job))
  invisible(NULL)
}
