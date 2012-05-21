#' Sets the job function for already existing jobs.
#'
#' Use this function only as last measure when there is a bug
#' in a part of your job function and you have already computed a large number
#' of (unaffected) results. This function allows you to fix the error and to
#' associate the jobs with the corrected function.
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
#' @return Nothing.
#' @export
setJobFunction = function(reg, ids, fun, more.args=list()) {
  UseMethod("setJobFunction")
}

#' @S3method setJobFunction Registry
setJobFunction.Registry = function(reg, ids, fun, more.args=list()) {
  checkArg(fun, cl="function")
  checkMoreArgs(more.args)

  if (missing(ids)) {
    ids = dbGetJobIds(reg)
  } else {
    ids = checkIds(reg, ids)
    if (!setequal(ids, dbGetJobIds(reg)))
      stop("Subsetting ids not yet supported")
  }

  fun.id = saveFunction(reg, fun, more.args)
  dbSetJobFunction(reg, ids, fun.id)
  invisible(NULL)
}
