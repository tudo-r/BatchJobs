#' Get log file paths for jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#' @return [\code{character}]. Ids of jobs.
#' @export
getLogFiles = function(reg, ids) {
  checkArg(reg, "Registry")
  ids = convertIntegers(ids)
  checkArg(ids, "integer", min.len=1L, na.ok=FALSE)
  fids = dbGetFirstJobInChunkIds(reg, ids)
  # if no chunking, use normal id
  j = is.na(fids)
  fids[j] = ids[j]
  vapply(fids, getLogFilePath, character(1L), reg=reg)
}
