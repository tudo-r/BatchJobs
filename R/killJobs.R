#' Kill some jobs on the batch system.
#'
#' @description
#' Kill jobs which have already been submitted to the batch system.
#' If a job is killed its internal state is reset as if it had not been submitted at all.
#'
#' The function informs if
#' (a) the job you want to kill has not been submitted,
#' (b) the job has already terminated,
#' (c) for some reason no batch job id is available.
#' In all 3 cases above, nothing is changed for the state of this job and no call
#' to the internal kill cluster function is generated.
#'
#' In case of an error when killing, the function tries - after a short sleep - to kill the remaining
#' batch jobs again. If this fails again for some jobs, the function gives up. Only jobs that could be
#' killed are reset in the DB.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @template arg_progressbar
#' @return [\code{integer}]. Ids of killed jobs.
#' @export
#' @family debug
#' @examples
#' \dontrun{
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) Sys.sleep(x)
#' batchMap(reg, f, 1:10 + 5)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # kill all jobs currently _running_
#' killJobs(reg, findRunning(reg))
#' # kill all jobs queued or running
#' killJobs(reg, findNotTerminated(reg))
#' }
killJobs = function(reg, ids, progressbar = TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids))
    return(invisible(integer(0L)))
  else
    ids = checkIds(reg, ids)
  assertFlag(progressbar)

  conf = getBatchJobsConf()
  killfun = getKillJob("Cannot kill jobs")

  # FIXME select and order (see below) could be done more efficiently in SQLite
  data = dbGetJobStatusTable(reg, ids = dbFindOnSystem(reg, ids),
                             cols = c("job_id", "batch_job_id", "submitted", "started", "done", "error"))

  # print first summary information on jobs to kill
  info("Trying to kill %i jobs.", length(ids))
  info("Jobs on system: %i", nrow(data))
  info("Of these: %i not submitted, %i with no batch.job.id, %i already terminated",
    sum(is.na(data$submitted)), sum(is.na(data$batch_job_id)), sum(!is.na(data$done) | !is.na(data$error)))


  # subset data: restrict to jobs submitted, not done, no error, has bji
  # other jobs can be ignored -> overwrite ids
  data = subset(data, !is.na(data$submitted) & is.na(data$done) & is.na(data$error) & !is.na(data$batch_job_id),
                select = c("job_id", "batch_job_id", "started"))

  # kill queued jobs first, otherwise they might get started while killing running jobs
  data = data[order(data$started, na.last = FALSE),, drop = FALSE]

  ids = data$job_id
  bjids = unique(data$batch_job_id)
  info("Killing real batch jobs: %i", length(bjids))

  if (length(bjids) == 0L) {
    info("No batch jobs to kill.")
    return(invisible(integer(0L)))
  }

  doKill = function(bjids) {
    n = length(bjids)
    old.warn = getOption("warn")
    bar = getProgressBar(progressbar, min = 0L, max = n, label = "killJobs")
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
  info(clipString(collapse(bjids), 200L, ",..."))
  bjids.notkilled = doKill(bjids)

  # second try to kill
  if (length(bjids.notkilled) > 0L) {
    info("Could not kill %i batch jobs, trying again.", length(bjids.notkilled))
    Sys.sleep(2)
    bjids.notkilled = doKill(bjids.notkilled)
  }

  # second try also not successful
  if (length(bjids.notkilled) > 0L) {
    fn = file.path(reg$file.dir, sprintf("killjobs_failed_ids_%i", now()))
    warningf("Could not kill %i batch jobs, kill them manually!\nTheir ids have been saved in %s.",
      length(bjids.notkilled), fn)
      writeLines(as.character(bjids.notkilled), con = fn)
  }

  # reset killed jobs
  ids = ids[data$batch_job_id %nin% bjids.notkilled]
  info("Resetting %i jobs in DB.", length(ids))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids, type = "last"))
  invisible(ids)
}
