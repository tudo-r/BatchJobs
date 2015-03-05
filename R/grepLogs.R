#' Grep log files for a pattern.
#'
#' @description
#' Searches for occurence of \code{pattern} in log files.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to grep.
#'   Default is all terminated jobs (done + errors).
#' @param pattern [\code{character(1)}]\cr
#'   Pattern to search for. See \code{\link{grep}}.
#'   Default is \code{"warn"}.
#' @param ignore.case [\code{logical(1)}]\cr
#'   Ignore case. See \code{\link{grep}}.
#'   Default is \code{TRUE}.
#' @param verbose [\code{logical(1)}]\cr
#'   Print matches.
#'   Default is \code{FALSE}.
#' @param range [\code{integer(1)}]\cr
#'   If \code{verbose} is set to \code{TRUE}, print \code{range}
#'   leading and trailing lines for contextual information about the warning.
#'   Default is \code{2}.
#' @return [\code{integer}]. Ids of jobs where pattern was found in the log file.
#' @family debug
#' @export
grepLogs = function(reg, ids, pattern = "warn", ignore.case = TRUE, verbose = FALSE, range = 2L) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindTerminated(reg)
  } else {
    nterminated = dbFindTerminated(reg, ids, negate = TRUE, limit = 1L)
    if (length(nterminated) > 0L)
      stopf("Not all jobs with provided ids have finished yet and therefore possess no log file, e.g. id=%i.",
            nterminated[1L])
  }
  assertString(pattern)
  assertFlag(ignore.case)
  assertFlag(verbose)
  range = asCount(range)

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

    # read lines from log and trim to output of job with id 'ids[i]'
    lines = readLines(fns[i])
    start = grep(sprintf("^########## Executing jid=%i ##########$", ids[i]), lines)
    if (length(start) != 1L)
      stop("The output of the job with id=%i could not be found in file '%s' or was found more than once", ids[i], fns[i])
    end = head(grep("^########## Executing jid = [0-9]+ ##########$", tail(lines, -start)), 1L)
    lines = lines[start:min(start+end, length(lines))]

    matches = grep(pattern, lines, ignore.case = ignore.case)
    matched[i] = (length(matches) > 0L)
    if (verbose && matched[i]) {
      messagef("\n##### Matches for job with id=%i (%s) #####",
               ids[i], basename(fns[i]))
      message(collapse(vcapply(matches, getLines, lines = lines, range = range), "\n---\n"))
    }
  }

  ids[matched]
}
