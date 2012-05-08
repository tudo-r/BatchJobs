#' Find jobs for which results are available.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findDone = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbGetDone(reg, ids)
}

#' Find jobs for which results are still missing.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findMissingResults = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbGetMissingResults(reg, ids)
}

#' Find jobs where errors occured.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findErrors = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbGetErrors(reg, ids)
}

#' Find jobs which have been submitted in the past.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findSubmitted = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbGetSubmitted(reg, ids)
}

#' Find jobs which are present on the batch system at the moment.
#'
#' Find jobs either queued, running, held, etc.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findOnSystem = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun = getListJobs("Cannot find jobs on system")
  batch.job.ids = fun(getBatchJobsConf(), reg)
  dbGetJobIdsFromBatchJobIds(reg, batch.job.ids, ids)
}


#' Find jobs which are running.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findRunning = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun = getListJobs("Cannot find running jobs")
  batch.job.ids = fun(getBatchJobsConf(), reg)
  # running jobs are running on batch system in general and must have started for this reg
  # also not terminated
  dbGetJobIdsFromBatchJobIds(reg, batch.job.ids, ids, "started IS NOT NULL AND done is NULL AND error is NULL")
}

#' Find jobs where walltime was probably hit.
#'
#' Right now the heuristic is as follows:
#' Find all jobs that have submitted, did not abort with an error,
#' did not complete with a result and are not submitted or running anymore.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
findExpired = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun = getListJobs("Cannot find expired jobs")
  batch.job.ids = fun(getBatchJobsConf(), reg)
  dbGetExpiredJobs(reg, batch.job.ids, ids)
}
