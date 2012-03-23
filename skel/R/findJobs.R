#' Find jobs for which results are available.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findDone = function(reg) {
  checkArg(reg, cl = "Registry")
  dbGetDone(reg)
}

#' Find jobs for which results are still missing.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findMissingResults = function(reg) {
  checkArg(reg, cl = "Registry")
  dbGetMissingResults(reg)
}

#' Find jobs where errors occured.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findErrors = function(reg) {
  checkArg(reg, cl = "Registry")
  dbGetErrors(reg)
}

#' Find jobs which have been submitted in the past.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findSubmitted = function(reg) {
  checkArg(reg, cl = "Registry")
  dbGetSubmitted(reg)
}

#' Find jobs which are present on the batch system at the moment.
#' 
#' Find jobs either queued, running, held, etc.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findOnSystem = function(reg) {
  checkArg(reg, cl = "Registry")
  fun = getListJobs("Cannot find jobs on system")
  batch.job.ids = fun(reg)
  dbGetJobIdsFromBatchJobIds(reg, batch.job.ids)
}


#' Find jobs which are running.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findRunning = function(reg) {
  checkArg(reg, cl = "Registry")
  fun = getListJobs("Cannot find running jobs")
  batch.job.ids = fun(reg)
  # running jobs are running on batch system in general and must have started for this reg
  dbGetJobIdsFromBatchJobIds(reg, batch.job.ids, "started IS NOT NULL")
}

#' Find jobs where walltime was probably hit.
#' 
#' Right now the heuristic is as follows:
#' Find all jobs that have submitted, did not abort with an error, 
#' did not complete with a result and are not submitted or running anymore.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findExpired = function(reg) {
  checkArg(reg, cl = "Registry")
  fun = getListJobs("Cannot find expired jobs")
  batch.job.ids = fun(reg)
  dbGetExpiredJobs(reg, batch.job.ids)
}
