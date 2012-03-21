#' Get execution times of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param use.names [\code{logical(1)}]
#'   Should result vector be named with \code{ids}?
#'   Default is \code{TRUE} if length of \code{ids} is more than 1.
#' @return [\code{integer}]. Time in seconds for jobs, named with ids.
#'   \code{NA} if job has not terminated successfully.
#' @export
getJobTimes = function(reg, ids, use.names=TRUE) {
  checkArg(reg, cl = "Registry")
  if (missing(ids))
    ids = getJobIds(reg)
  else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
  }
  if (missing(use.names)) {
    use.names = length(ids) > 1L
  } else {
    checkArg(use.names, "logical", len=1, na.ok=FALSE)
  }
  d = dbGetJobTimes(reg, ids)
  times = d$done - d$start
  if (use.names)
    names(times) = ids
  return(times)
}
