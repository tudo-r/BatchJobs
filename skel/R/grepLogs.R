#' Grep log files for a pattern.
#'
#' Searches for occurence of \code{pattern} in log files.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to grep.
#'   Default is all done jobs.
#' @param pattern [\code{character(1L)}]\cr
#'   Pattern to search for. See \code{\link{grep}}.
#'   Default is \code{"warn"}.
#' @param ignore.case [\code{logical(1L)}]\cr
#'   Ignore case. See \code{\link{grep}}.
#'   Default is \code{TRUE}.
#' @param verbose [\code{logical(1L)}]\cr
#'   Print matches.
#'   Default is \code{FALSE}.
#' @param range [\code{integer(1L)}]\cr
#'   If \code{verbose} is set to \code{TRUE}, print \code{range}
#'   leading and trailing lines for contextual information about the warning.
#'   Default is \code{2}.
#' @return [\code{integer}]. Ids of jobs where pattern was found in the log file.
#' @export
grepLogs = function(reg, ids, pattern="warn", ignore.case=TRUE, verbose=FALSE, range=2L) {
  checkArg(reg, "Registry")
  if (missing(ids)) {
    ids = dbGetDone(reg, ids)
  } else {
    ids = checkIds(reg, ids)
    if (! all(ids %in% dbGetDone(reg)))
      stop("Not all jobs with provided ids have finished yet and therefore possess no log file")
  }
  checkArg(pattern, "character", len=1L, na.ok=FALSE)
  checkArg(ignore.case, "logical", len=1L, na.ok=FALSE)
  checkArg(range, "integer", len=1L, lower=0L, na.ok=FALSE)

  fids = dbGetFirstJobInChunkIds(reg, ids)
  fns = getLogFilePath(reg, ifelse(is.na(fids), ids, fids))
  matched = logical(length(ids))

  getLines = function(lines, match, range) {
    start = max(1L, match - range)
    stop = min(length(lines), match + range)
    collapse(lines[start:stop], "\n")
  }

  for(i in seq_along(fns)) {
    if (!file.exists(fns[i]))
      stopf("File '%s' does not exist.", fns[i])
    lines = readLines(fns[i])
    matches = grep(pattern, lines, ignore.case=ignore.case)
    matched[i] = (length(matches) > 0L)
    if (verbose && matched[i]) {
      messagef("%s##### Matches for job with id=%i (%s) #####",
               ifelse(i >= 2L, "\n", ""), ids[i], basename(fns[i]))
      message(collapse(vapply(matches, getLines, character(1L), lines=lines, range=range),
                       "\n---\n"))
    }
  }

  ids[matched]
}
