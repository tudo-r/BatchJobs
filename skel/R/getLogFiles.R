#' Get log file paths for jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#' @return [\code{character}]. Ids of jobs.
#' @export
getLogFiles = function(reg, ids) {
  checkArg(reg, "Registry")
  ids = checkIds(reg, ids)
  fids = dbGetFirstJobInChunkIds(reg, ids)
  vapply(ifelse(is.na(fids), ids, fids), getLogFilePath, character(1L), reg=reg)
}
