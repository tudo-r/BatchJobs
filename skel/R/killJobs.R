#' Kill a job on the batch system.
#'
#' Kill jobs which have already been submitted to the batch system.
#' If a job is killed its internal state is reset as if it had not been submitted at all.
#'
#' The function informs if
#' (a) the job you want to kill has not been submitted,
#' (b) the job has already terminated,
#' (c) for some reason no batch job is is available.
#' In all 3 cases above nothing is changed for the state of this job and no call
#' to the internal kill cluster function is generated.
#'
#' In case of an error when killing, the function tries after a short sleep to kill the remaining
#' batch jobs again. If this fails again for some jobs, the function gives up. Only jobs that could be
#' killed are reset in the DB.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @return Vector of type \code{integer} with ids of killed jobs.
#' @export
killJobs = function(reg, ids) {
  checkArg(reg, cl="Registry")
  if (missing(ids))
    return(invisible(integer(0L)))
  else
    ids = checkIds(reg, ids)

  printInfo = function() {
    messagef("Trying to kill %i jobs.", length(ids))
    ids.onsys = findOnSystem(reg, ids)
    data = dbGetJobStatusTable(reg, ids.onsys)
    n.unsubm = sum(is.na(data$submitted))
    n.nobji = sum(is.na(data$batch_job_id))
    n.term = sum(!is.na(data$done) | !is.na(data$error))
    messagef("Jobs on system: %i", length(ids.onsys))
    messagef("Of these: %i not submitted, %i with no batch.job.id, %i already terminated",
     n.unsubm, n.nobji, n.term)
    messagef("Killing real batch jobs: %i", length(ids.batch))
  }

  printBjis = function() {
    # show first batch.job.ids
    cutoff = cumsum(nchar(ids.batch) + 1L) > 200L
    if (any(cutoff))
      ids.str = paste(collapse(head(ids.batch, head(which(cutoff), 1L))), ",...", sep="")
    else
      ids.str = collapse(ids.batch)
    message(ids.str)
  }

  killThem = function(bjis) {
    old.warn = getOption("warn")
    options(warn=0L)
    bar = makeProgressBar(min=0L, max=length(bjis), label="killJobs")
    bar$set()
    notkilled = character(0L)
    for (i in seq_along(bjis)) {
      bji = bjis[i]
      ok = try(killfun(conf, reg, bji))
      if (is.error(ok)) {
        notkilled = c(notkilled, bji)
        warning(as.character(ok))
      }
      bar$inc(1L)
    }
    bar$kill()
    options(warn=old.warn)
    return(notkilled)
  }

  conf = getBatchJobsConf()
  killfun = getKillJob("Cannot kill jobs")
  ids.onsys = findOnSystem(reg, ids)
  data = dbGetJobStatusTable(reg, ids.onsys)
  # must be submitted, be not done, no error, has bji
  data.subset = subset(data, !is.na(data$submitted) & is.na(data$done)
    & is.na(data$error) & !is.na(data$batch_job_id))
  # unique because of chunking
  ids.batch = unique(data.subset$batch_job_id)
  ids.job = data.subset$job_id

  printInfo()
  if (length(ids.batch) == 0L) {
    message("No batch jobs to kill.")
    return(invisible(integer(0L)))
  }

  if (length(ids.batch) > 0L) {
    printBjis()
    notkilled = killThem(ids.batch)
    if (length(notkilled) > 0L) {
      messagef("Could not kill %i batch jobs, trying again.", length(notkilled))
      Sys.sleep(2)
      notkilled = killThem(notkilled)
    }
  }
  if (length(notkilled) > 0L) {
    messagef("Could not kill %i batch jobs, kill them manually!\nTheir ids have been saved in .GlobalEnv under .batch.job.ids", length(notkilled))
    assign(".batch.job.ids", notkilled, envir=.GlobalEnv)
    # only reset killed jobs
    ids.job = subset(data.subset, data.subset$batch_job_id %nin% notkilled)
  }

  messagef("Resetting %i jobs in DB.", length(ids.job))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids.job))
  invisible(ids.job)
}
