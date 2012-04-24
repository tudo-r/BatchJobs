#' Display the contents of a log file.
#'
#' Display the contents of a log file, useful in case of errors.
#'
#' Note this rare special case: When you use chunking, submit some jobs, some jobs fail,
#' then you resubmit these jobs again in different chunks, the log files will contain the log
#' of the old, failed job as well. But \code{showLog} will always jump to the correct part
#' of the new log file.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of selected job.
#'   Default is first id in registry.
#' @return [\code{character(1)}]. Invisibly returns path to log file.
#' @export
showLog = function(reg, id) {
  checkArg(reg, "Registry")
  if (missing(id)) {
    id = dbGetJobId(reg)
    if (length(id) == 0L)
      stop("No jobs in registry!")
  } else {
    id = checkId(reg, id)
  }

  fn = getLogFiles(reg, id)
  if (!file.exists(fn))
    stopf("Log file does not exist: %s", fn)
  exact1 = sprintf("Executing jid=%i", id)
  exact2 = sprintf("########## %s ##########", exact1)
  header = c(sprintf("Showing log file for job with id=%i", id),
             sprintf("The string '%s' indicates start of requested job", exact1))

  pager = getOption("pager")
  sys.pager = Sys.getenv("PAGER")
  if (grepl("/pager$", pager) && sys.pager != "") {
    # no user configured pager, we use the system pager
    pager = sys.pager
  }
  bn = basename(strsplit(pager, " ", fixed=TRUE)[[1L]][1L])

  # check for less or vim as pagers
  # if we find the pattern, we jump to the matching line
  if (bn %in% c("less", "vim")) {
    pos = grep(exact2, readLines(fn), fixed=TRUE)
    if (length(pos) == 1L) {
      pos = pos + length(header) + 1L
      if(pager == "less")
        pager = sprintf("%s +%ig", pager, pos)
      else
        pager = sprintf("%s +%i", pager, pos)
    }
  }

  file.show(fn, pager = pager, header=collapse(header, sep="\n"))

  invisible(fn)
}
