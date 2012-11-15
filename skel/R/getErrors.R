#' Get error messages of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param print [\code{logical(1)}]\cr
#'   Print error messages?
#'   Default is \code{TRUE}.
#' @return [\code{character}]. Error messages for jobs as character vector invisibly returned.\cr
#'   \code{NA} if job has terminated successfully.
#' @export
#' @seealso \code{\link{showLog}} for more contextual error informations.
getErrors = function(reg, ids, print=TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  checkArg(print, "logical", len=1L, na.ok=FALSE)

  tab = dbGetErrorMsgs(reg, ids, filter=FALSE)
  msgs = setNames(tab$error, tab$job_id)
  tab = tab[!is.na(tab$error),, drop=FALSE]

  if (print) {
    if (nrow(tab) == 0L) {
      catf("No errors")
    } else {
      catf("Showing %i errors:", nrow(tab))
      cat(sprintf("Error in %i: %s", tab$job_id, tab$error), sep = "\n")
    }
  }

  invisible(msgs)
}
