#' Get job from registry by id.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param load.fun [\code{logical(1)}]\cr
#'   Load job function from disk?
#'   Default is \code{FALSE}.
#' @param check.id [\code{logical(1)}]\cr
#'   Check the job id?
#'   Default is \code{TRUE}.
#' @return [\code{Job}].
#' @export
getJob = function(reg, id, load.fun=FALSE, check.id=TRUE) {
  getJobs(reg, id, load.fun, check.ids=check.id)[[1L]]
}

#' Get jobs from registry by id.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#' @param load.fun [\code{logical(1)}]\cr
#'   Load job function from disk?
#'   Default is \code{FALSE}.
#' @param check.ids [\code{logical(1)}]\cr
#'   Check the job ids?
#'   Default is \code{TRUE}.
#' @return [list of \code{\link{Job}}].
#' @export
getJobs = function(reg, ids, load.fun=FALSE, check.ids=TRUE) {
  UseMethod("getJobs")
}

#' @method getJobs Registry
#' @S3method getJobs Registry
getJobs.Registry = function(reg, ids, load.fun=FALSE, check.ids=TRUE) {
  if (check.ids) {
    if (length(ids) == 0L)
      return(list())
    checkIds(reg, ids)
  }

  jobs = dbGetJobs(reg, ids)
  if (load.fun)
    return(lapply(jobs, loadJobFunction, reg=reg))
  return(jobs)
}
