#' Get log file paths for jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @return [\code{character}]. Vector of file paths to log files.
#' @export
getLogFiles = function(reg, ids) {
  checkArg(reg, "Registry")
  if (missing(ids))
    ids = getJobIds(reg)
  else
    ids = checkIds(reg, ids)
  fids = dbGetFirstJobInChunkIds(reg, ids)
  vapply(ifelse(is.na(fids), ids, fids), getLogFilePath, character(1L), reg=reg)
}
