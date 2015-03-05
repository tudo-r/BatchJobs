#' Display the contents of a log file.
#'
#' @description
#' Display the contents of a log file, useful in case of errors.
#'
#' Note this rare special case: When you use chunking, submit some jobs, some jobs fail,
#' then you resubmit these jobs again in different chunks, the log files will contain the log
#' of the old, failed job as well. \code{showLog} tries to jump to the correct part
#' of the new log file with a supported pager.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of selected job.
#'   Default is first id in registry.
#' @param pager [\code{any}]\cr
#'   Pager to use to display the log. Defaults to \code{getOption("pager")}.
#'   This option is passed to \code{file.show} and is highly OS dependant and GUI dependant.
#'   If either R's pager or the environment variable \dQuote{PAGER} is set to \dQuote{less}
#'   or \dQuote{vim}, the correct part of the log file will be shown.
#'   Otherwise you find information about the correct part in the beginning of the displayed file.
#' @return [\code{character(1)}]. Invisibly returns path to log file.
#' @family debug
#' @export
showLog = function(reg, id, pager = getOption("pager")) {
  checkRegistry(reg)
  syncRegistry(reg)
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

  if (!is.function(pager)) {
    sys.pager = Sys.getenv("PAGER")
    if ((grepl("/pager$", pager) || pager == "internal") && sys.pager != "") {
      # prefer the system pager
      pager = sys.pager
    }
    bn = basename(strsplit(pager, " ", fixed = TRUE)[[1L]][1L])

    # check for less or vim as pager
    # if we find the pattern, we jump to the matching line
    if (bn %in% c("less", "vim")) {
      pos = grep(exact2, readLines(fn), fixed = TRUE)
      if (length(pos) == 1L) {
        pos = pos + length(header) + 1L
        if (pager == "less")
          pager = function(files, header, title, delete.file) system2("less", c(sprintf("+%ig", pos), files))
        else
          pager = function(files, header, title, delete.file) system2("vim", c(sprintf("+%i", pos), files))
      }
    }
  }

  file.show(fn, pager = pager, header = collapse(header, sep = "\n"))
  invisible(fn)
}
