#' Sets the job function for already existing jobs.
#'
#' @description
#' Use this function only as last measure when there is a bug
#' in a part of your job function and you have already computed a large number
#' of (unaffected) results. This function allows you to fix the error and to
#' associate the jobs with the corrected function.
#'
#' Note that by default the computational state of the affected jobs is also reset.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param fun [\code{function}]\cr
#'   Replacement function.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @param reset [\code{logical(1)}]\cr
#'   Reset job status via \code{\link{resetJobs}}.
#'   Default is \code{TRUE}.
#' @param force [\code{logical(1)}]\cr
#'   See \code{\link{resetJobs}}.
#'   Default is \code{FALSE}.
#' @return Nothing.
#' @family debug
#' @export
setJobFunction = function(reg, ids, fun, more.args = list(), reset = TRUE, force = FALSE) {
  checkRegistry(reg, strict = TRUE)
  syncRegistry(reg)
  assertFunction(fun)
  checkMoreArgs(more.args)
  assertFlag(reset)
  assertFlag(force)
  UseMethod("setJobFunction")
}

#' @export
setJobFunction.Registry = function(reg, ids, fun, more.args = list(), reset = TRUE, force = FALSE) {
  if (missing(ids)) {
    # nothing to do ...
    return(invisible(NULL))
  } else {
    ids = checkIds(reg, ids)
  }
  fun.id = saveFunction(reg, fun, more.args)
  dbSetJobFunction(reg, ids, fun.id)
  if (reset)
    resetJobs(reg, ids, force = force)
  invisible(NULL)
}
