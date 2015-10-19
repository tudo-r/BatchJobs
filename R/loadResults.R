#' Loads result files for id vector.
#' @template arg_reg
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all done jobs.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified to a vector, matrix or higher dimensional array if possible?
#'   Default is \code{TRUE}.
#' @param use.names [\code{character(1)}]\cr
#'   Name the results with job ids (\dQuote{ids}), stored job names (\dQuote{names})
#'   or return a unnamed result (\dQuote{none}).
#'   Default is \code{ids}.
#' @param missing.ok [\code{logical(1)}]\cr
#'   If \code{FALSE} an error is thrown if the results are not found.
#'   Otherwise missing results are imputed to \code{NULL}.
#'   Default is \code{FALSE}.
#' @param impute.val [any]\cr
#'   The value to return when no result is available.
#'   Defaults to \code{NULL} (the previous behavior)
#' @return [\code{list}]. Results of jobs as list, possibly named by ids.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResults = function(reg, ids, part = NA_character_, simplify = FALSE, use.names = "ids", missing.ok = FALSE, impute.val = NULL) {
  checkRegistry(reg, writeable = FALSE)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindDone(reg)
  } else {
    ids = checkIds(reg, ids)
  }
  checkPart(reg, part)
  assertFlag(simplify)
  use.names = convertUseNames(use.names)
  assertFlag(missing.ok)

  res = getResults(reg, ids, part, missing.ok, impute.val = impute.val)
  names(res) = switch(use.names,
                      "none" = NULL,
                      "ids" = as.character(ids),
                      "names" = dbGetJobNames(reg, ids))
  if(simplify && length(res) > 0L)
    res = simplify2array(res, higher = (simplify=="array"))

  return(res)
}

getResults = function(reg, ids, part = NA_character_, missing.ok = FALSE, impute.val = NULL) {
  if (reg$multiple.result.files) {
    read.files = function(id, dir, pattern) {
      fns = list.files(dir, pattern, full.names = TRUE)
      found.parts = sub(".+-(.+)\\.RData$", "\\1", basename(fns))
      if(length(found.parts) == 0L) {
        if (missing.ok)
         return(list())
        stop("No partial result files found for job with id ", id)
      }

      setNames(lapply(fns, load2, "result"), found.parts)
    }

    dirs = getJobDirs(reg, ids)
    if(length(part) == 1L && is.na(part)) {
      patterns = sprintf("^%i-result-.+\\.RData$", ids)
    } else {
      patterns = sprintf("^%i-result-(%s)\\.RData$", ids, collapse(part, "|"))
    }

    return(mapply(read.files, id = ids, dir = dirs, pattern = patterns, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  }

  fns = getResultFilePath(reg, ids, part)
  miss = !file.exists(fns)

  if (any(miss)) {
    if (!missing.ok)
      stopf("Some job result files do not exist, showing up to first 10:\n%s", collapse(head(fns[miss], 10L), "\n"))
    ret = rep.int(list(impute.val), times = length(ids))
    ret = replace(ret, !miss, lapply(fns[!miss], load2, "result"))
  } else {
    ret = lapply(fns, load2, "result")
  }
  ret
}
