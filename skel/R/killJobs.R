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
  ids.kill = integer(0L)
  data = subset(data, data$job_id %in% ids)
  n.unsubm = sum(is.na(data$submitted))
  n.term = sum(!is.na(data$done) | !is.na(data$error))
  n.nobji = sum(!is.na(data$submitted) & is.na(data$batch_job_id))
  messagef("Not submitted: %i", n.unsubm)
  messagef("Already terminated: %i", n.term)
  messagef("No batch.job.id: %i", n.term)
  # must submitted, be not done, no error, has bji
  data.bji = subset(data, !is.na(data$submitted) & is.na(data$done) 
    & is.na(data$error) & !is.na(data$batch_job_id))
  # unique because of chunking  
  bjis = unique(data.bji$batch_job_id)
  ids.kill = data.bji$job_id
  n.bjis = length(bjis)
  messagef("Killing batch.job.ids: %i", n.bjis)
  if (n.bjis > 0L) {
    bjis.str = collapse(bjis)
    # trim string so its not too long
    if (str_length(bjis.str) > 200L)
      bjis.str = sprintf("%s...", str_sub(bjis.str, end=200L))
    messagef(bjis.str)
    conf = getBatchJobsConf()
    lapply(bjis, killfun, reg=reg, conf=conf)
  }
  messagef("For safety reasons waiting 3secs...")
  Sys.sleep(3)
  messagef("Resetting %i jobs in DB.", length(ids.kill))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids.kill))
  invisible(NULL)
}
