#' Get error messages of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs with errors.
#' @return [\code{character}]. Error messages for jobs as character vector\cr
#'   \code{NA} if job has terminated successfully.
#' @family debug
#' @export
getErrorMessages = function(reg, ids) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids))
    ids = dbFindErrors(reg)
  else
    ids = checkIds(reg, ids)

  tab = dbGetErrorMsgs(reg, ids, filter = FALSE)
  setNames(tab$error, tab$job_id)
}
