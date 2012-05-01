#' Get execution times of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param use.names [\code{logical(1)}]\cr
#'   Should result vector be named with \code{ids}?
#'   Default is \code{TRUE}.
#' @return [\code{integer}]. Time in seconds for jobs, named with ids.
#'   \code{NA} if job has not terminated successfully.
#' @export
getJobTimes = function(reg, ids, use.names=TRUE) {
  checkArg(reg, cl = "Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  checkArg(use.names, "logical", len=1L, na.ok=FALSE)

  tab = dbGetJobTimes(reg, ids)
  if (!use.names)
    return(tab$time)

  times = tab$time
  names(times) = tab$job_id
  return(times)
}
