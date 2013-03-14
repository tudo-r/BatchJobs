#' Sweep obsolete files from the file system
#'
#' Removes R scripts, log files, resource informations and configuration files
#' from the registry's file directory.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param sweep [character]\cr
#'   Vector with the possible choices \dQuote{scripts}, \dQuote{logs}, \dQuote{resources}
#'   and \dQuote{conf}, or the string \dQuote{all} as a shortcut for all possible choices.
#'   Default is \dQuote{scripts} which removes all \code{R} script files.
#' @return [\code{logical}] Invisibly returns \code{TRUE} on success, \code{FALSE}
#'   if some files could not be removed.
#' @export
sweepRegistry = function(reg, sweep = "scripts") {
  checkRegistry(reg)
  syncRegistry(reg)

  checkArg(sweep, "character", min.len=1L, na.ok=FALSE)
  choices = c("scripts", "logs", "resources", "conf")
  if (length(sweep) == 1L && sweep == "all")
    sweep = choices
  else
    sweep = match.arg(sweep, choices = choices, several.ok = TRUE)

  if (length(dbFindRunning(reg)))
    stop("Can't sweep registry while jobs are running")

  fd = reg$file.dir
  jd = file.path(fd, "jobs")
  rd = file.path(fd, "resources")

  # failed kill ids are always obsolete because no jobs are running anymore
  files = list.files(fd, pattern = "^killjobs_failed_ids_*", full.names = TRUE)

  # sweep configuration
  if ("conf" %in% sweep)
    files = c(files, list.files(fd, pattern = "^conf.RData$", full.names = TRUE))

  # sweep resources
  if ("resources" %in% sweep)
    files = c(files, list.files(rd, full.names = TRUE))

  # sweep logs and scripts (in one go if possible)
  if (all(c("logs", "scripts") %in% sweep)) {
    files = c(files, list.files(jd, pattern = "^[0-9]+\\.[out|R]$", recursive = TRUE, full.names = TRUE))
  } else {
    if ("logs" %in% sweep)
      files = c(files, list.files(jd, pattern = "^[0-9]+\\.out$", recursive = TRUE, full.names = TRUE))
    if ("scripts" %in% sweep)
      files = c(files, list.files(jd, pattern = "^[0-9]+\\.R$", recursive = TRUE, full.names = TRUE))
  }

  messagef("Removing %i files ...", length(files))
  ok = all(file.remove(files))
  if (!ok)
    warning("Not all files could be deleted. Check file permissions and try again")
  return(invisible(ok))
}
