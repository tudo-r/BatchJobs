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
#' @examples \dontrun{
#'  reg <- makeRegistry(id="BatchJobsExample", seed=123)
#'  f <- function(x) x^2
#'  batchMap(reg, f, 1:10)
#'  submitJobs(reg)
#'  # what is 2^2:
#' loadResult(reg, 2)
#' }
loadResult = function(reg, id, part=as.character(NA), check.id=TRUE) {
  checkArg(reg, cl="Registry")
  if(check.id) {
    id = convertInteger(id)
    checkArg(id, "integer", len=1L, na.ok=FALSE)
    checkIds(reg, id)
  }
  ee = new.env()

  if (reg$multiple.result.files) {
    fn = list.files(BatchJobs:::getJobDir(reg, id),
                    pattern=sprintf("^%i-result-.+\\.RData$", id),
                    full.names=TRUE)
    names(fn) = sub(".+-(.+)\\.RData$", "\\1", fn)

    checkArg(part, "character", min.len=1L, na.ok=TRUE)
    if (length(part) > 1L || !is.na(part)) {
      #messagef("part is %s", collapse(part))
      fn = fn[names(fn) %in% part]
    }

    if (length(fn) == 0L)
      stop("No partial result files found for job with id ", id)

    result = namedList(names(fn))
    for(i in seq_along(result)) {
      #message("Loading partial job result file: ", fn[i])
      load(fn[i], envir=ee)
      result[[i]] = ee$result
    }
    return(result)
  } else {
    if (!is.na(part))
      stop("multiple.result.files is FALSE. You cannot specify 'part'!")

    fn = getResultFilePath(reg, id, part)
    #message("Loading job result file: ", fn)
    if (!file.exists(fn))
      stop("Job result file does not exist: ", fn)
    load(fn, envir=ee)
    return(ee$result)
  }
}
