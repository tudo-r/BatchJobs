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
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids))
    return(invisible(integer(0L)))
  else
    ids = checkIds(reg, ids)

  conf = getBatchJobsConf()
  killfun = getKillJob("Cannot kill jobs")
  data = dbGetJobStatusTable(reg, ids = findOnSystem(reg, ids), # FIXME registry is already checked and sync'd ...
                             cols = c("job_id", "batch_job_id", "submitted", "done", "error"))

  # print first summary information on jobs to kill
  messagef("Trying to kill %i jobs.", length(ids))
  messagef("Jobs on system: %i", nrow(data))
  messagef("Of these: %i not submitted, %i with no batch.job.id, %i already terminated",
           sum(is.na(data$submitted)), sum(is.na(data$batch_job_id)), sum(!is.na(data$done) | !is.na(data$error)))

  # subset data: restrict to jobs submitted, not done, no error, has bji
  # other jobs can be ignored -> overwrite ids
  data = subset(data, !is.na(data$submitted) & is.na(data$done) & is.na(data$error) & !is.na(data$batch_job_id),
                select = c("job_id", "batch_job_id"))
  ids = data$job_id
  bjids = unique(data$batch_job_id)
  messagef("Killing real batch jobs: %i", length(bjids))

  if (length(bjids) == 0L) {
    message("No batch jobs to kill.")
    return(invisible(integer(0L)))
  }

  doKill = function(bjids) {
    n = length(bjids)
    old.warn = getOption("warn")
    bar = makeProgressBar(min=0L, max=n, label="killJobs")
    on.exit({ options(warn = old.warn); bar$kill() })

    options(warn = 0L)
    bar$set()
    notkilled = logical(n)

    for (i in seq_len(n)) {
      ok = try(killfun(conf, reg, bjids[i]))
      if (is.error(ok)) {
        notkilled[i] = TRUE
        warning(as.character(ok))
      }
      bar$set(i)
    }

    bjids[notkilled]
  }

  # first try to kill
  message(shortenString(collapse(bjids), 200L, ",..."))
  bjids.notkilled = doKill(bjids)

  # second try to kill
  if (length(bjids.notkilled) > 0L) {
    messagef("Could not kill %i batch jobs, trying again.", length(bjids.notkilled))
    Sys.sleep(2)
    bjids.notkilled = doKill(bjids.notkilled)
  }

  # second try also not successful
  if (length(bjids.notkilled) > 0L) {
		fn = file.path(reg$file.dir, sprintf("killjobs_failed_ids_%i", now()))
    warningf("Could not kill %i batch jobs, kill them manually!\nTheir ids have been saved in %s.", 
			length(bjids.notkilled), fn)
		writeLines(as.character(bjids.notkilled), con=fn)
  }	

  # reset killed jobs
  ids = ids[data$batch_job_id %nin% bjids.notkilled]
  messagef("Resetting %i jobs in DB.", length(ids))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids), staged = FALSE)
  invisible(ids)
}
