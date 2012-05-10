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
  if (check.id)
    id = checkId(reg, id)
  getJobs(reg, id, load.fun, check.ids=FALSE)[[1L]]
}

#' Get jobs from registry by id.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param load.fun [\code{logical(1)}]\cr
#'   Load job function from disk?
#'   Default is \code{FALSE}.
#' @param check.ids [\code{logical(1)}]\cr
#'   Check the job ids?
#'   Default is \code{TRUE}.
#' @return [list of \code{\link{Job}}].
#' @export
getJobs = function(reg, ids, load.fun=FALSE, check.ids=TRUE) {
  checkArg(reg, "Registry")
  checkArg(load.fun, "logical", len=1L, na.ok=FALSE)
  checkArg(check.ids, "logical", len=1L, na.ok=FALSE)
  UseMethod("getJobs")
}


#' @method getJobs Registry
#' @S3method getJobs Registry
getJobs.Registry = function(reg, ids, load.fun=FALSE, check.ids=TRUE) {
  if (! missing(ids) && check.ids) {
    ids = checkIds(reg, ids)
  }

  jobs = dbGetJobs(reg, ids)
  if (!load.fun)
    return(jobs)

  fun.dir = getFunDir(reg$file.dir)
  fid = unique(extractSubList(jobs, "fun.id"))
  fn = file.path(fun.dir, sprintf("%s.RData", fid))
  loaded.stuff = lapply(fn, load2, parts = c("fun", "more.args"))
  names(loaded.stuff) = fid
  lapply(jobs, function(job) {
    x = loaded.stuff[[job$fun.id]]
    job$fun = x$fun
    job$more.args = x$more.args
    job
  })
}
