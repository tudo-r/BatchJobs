#' Get log file paths for jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @return [\code{character}]. Vector of file paths to log files.
#' @family debug
#' @export
getLogFiles = function(reg, ids) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids))
    ids = getJobIds(reg)
  else
    ids = checkIds(reg, ids)
  fids = dbGetFirstJobInChunkIds(reg, ids)
  getLogFilePath(reg, ifelse(is.na(fids), ids, fids))
}
