#' Loads result files for id vector.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all done jobs.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified to a vector, matrix or higher dimensional array if possible?
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should the returned list be named with the ids?
#'   Default is \code{TRUE}.
#' @return [\code{list}]. Results of jobs as list, possibly named by ids.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResults = function(reg, ids, part=NA_character_, simplify=FALSE, use.names=TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindDone(reg)
  } else {
    ids = checkIds(reg, ids)
  }
  checkPart(reg, part)
  checkArg(simplify, "logical", len=1L, na.ok=FALSE)
  checkArg(use.names, "logical", len=1L, na.ok=FALSE)

  res = getResults(reg, ids, part)
  if(use.names)
    names(res) = ids
  if(simplify && length(res) > 0L)
    res = simplify2array(res, higher = (simplify=="array"))

  return(res)
}

getResults = function(reg, ids, part=NA_character_) {
  if (reg$multiple.result.files) {
    read.files = function(id, dir, pattern) {
      fns = list.files(dir, pattern, full.names=TRUE)
      found.parts = sub(".+-(.+)\\.RData$", "\\1", basename(fns))
      if(length(found.parts) == 0L)
        stop("No partial result files found for job with id ", id)

      setNames(lapply(fns, load2, "result"), found.parts)
    }

    dirs = getJobDirs(reg, ids)
    if(length(part) == 1L && is.na(part)) {
      patterns = sprintf("^%i-result-.+\\.RData$", ids)
    } else {
      patterns = sprintf("^%i-result-(%s)\\.RData$", ids, collapse(part, "|"))
    }

    return(mapply(read.files, id = ids, dir=dirs, pattern=patterns, SIMPLIFY=FALSE, USE.NAMES=FALSE))
  }

  fns = getResultFilePath(reg, ids, part)
  miss = fns[!file.exists(fns)]
  if(length(miss) > 0L)
    stop("Job result file does not exist: ", fns)
  return(lapply(fns, load2, "result"))
}
