#' Get execution times of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @return [\code{integer}]. Time in seconds for jobs, named with ids.
#'   \code{NA} if job has not terminated successfully.
#' @export
getJobTimes = function(reg, ids) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)

  tab = dbGetJobTimes(reg, ids)
  times = tab$time
  names(times) = tab$job_id
  return(times)
}
