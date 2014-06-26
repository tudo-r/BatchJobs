#' Get job from registry by id.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param check.id [\code{logical(1)}]\cr
#'   Check the job id?
#'   Default is \code{TRUE}.
#' @return [\code{\link{Job}}].
#' @export
getJob = function(reg, id, check.id = TRUE) {
  if (check.id)
    id = checkId(reg, id)
  getJobs(reg, id, check.ids = FALSE)[[1L]]
}

#' Get jobs from registry by id.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param check.ids [\code{logical(1)}]\cr
#'   Check the job ids?
#'   Default is \code{TRUE}.
#' @return [list of \code{\link{Job}}].
#' @export
getJobs = function(reg, ids, check.ids = TRUE) {
  checkRegistry(reg)
  # syncRegistry(reg) NOT!
  assertFlag(check.ids)
  UseMethod("getJobs")
}

#' @method getJobs Registry
#' @export
getJobs.Registry = function(reg, ids, check.ids = TRUE) {
  if (! missing(ids) && check.ids) {
    ids = checkIds(reg, ids)
  }

  dbGetJobs(reg, ids)
}
