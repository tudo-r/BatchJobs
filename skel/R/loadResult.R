#' Loads a specific result file.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param part [\code{character(1)}]
#'   Only useful for multiple result files, then defines which result file part should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param check.id [\code{logical(1)}]\cr
#'   Check the job id?
#'   Default is \code{TRUE}.
#' @return [any]. Result of job.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResult = function(reg, id, part=as.character(NA), check.id=TRUE) {
  checkArg(reg, cl="Registry")
  if (check.id)
    id = checkId(reg, id)

  if (reg$multiple.result.files) {
    fn = list.files(BatchJobs:::getJobDirs(reg, id),
                    pattern=sprintf("^%i-result-.+\\.RData$", id),
                    full.names=TRUE)
    names(fn) = sub(".+-(.+)\\.RData$", "\\1", fn)

    checkArg(part, "character", min.len=1L, na.ok=TRUE)
    if (length(part) > 1L || !is.na(part)) {
      fn = fn[names(fn) %in% part]
    }

    if (length(fn) == 0L)
      stop("No partial result files found for job with id ", id)

    result = lapply(fn, function(fn) loadSingleObject(fn, "result"))
    names(result) = names(fn)
  } else {
    if (!is.na(part))
      stop("multiple.result.files is FALSE. You cannot specify 'part'!")

    fn = getResultFilePath(reg, id, part)
    if (!file.exists(fn))
      stop("Job result file does not exist: ", fn)
    result = loadSingleObject(fn, "result")
  }
  result
}
