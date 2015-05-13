#' @title Sweep obsolete files from the file system.
#'
#' @description
#' Removes R scripts, log files, resource informations and temporarily stored configuration files
#' from the registry's file directory. Assuming all your jobs completed successfully, none of these are needed
#' for further work. This operation potentially releases quite a lot of disk space, depending on the number of your jobs.
#' BUT A HUGE WORD OF WARNING:
#' IF you later notice something strange and need to determine the reason for it, you are at a huge disadvantage.
#' Only do this at your own risk and when you are sure that you have successfully completed a project and only
#' want to archive your produced experiments and results.
#'
#' @template arg_reg
#' @param sweep [\code{character}]\cr
#'   Possible choices:
#'   Temporary R scripts of jobs,
#'   really not needed for anything else then execution (\dQuote{scripts}),
#'   log file of jobs,
#'   think about whether you later want to inspect them (\dQuote{logs}),
#'   BatchJobs configuration files which are temporarily stored on submit,
#'   really not needed for anything else then execution (\dQuote{conf}),
#'   resource lists of \code{\link{submitJobs}} which are temporarily stored on submit,
#'   think about whether you later want to inspect them (\dQuote{resources}),
#'   Default is \code{c("scripts", "conf")}.
#' @return [\code{logical}]. Invisibly returns \code{TRUE} on success and \code{FALSE}
#'   if some files could not be removed.
#' @export
sweepRegistry = function(reg, sweep = c("scripts", "conf")) {
  checkRegistry(reg)
  syncRegistry(reg)

  assertSubset(sweep, c("scripts", "logs", "resources", "conf"))

  if (length(dbFindRunning(reg, limit = 1L)) > 0L)
    stop("Can't sweep registry while jobs are running")

  fd = reg$file.dir
  jd = getJobParentDir(fd)
  rd = getResourcesDir(fd)

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

  info("Removing %i files ...", length(files))
  ok = all(file.remove(files))
  if (!ok)
    warning("Not all files could be deleted. Check file permissions and try again")
  return(invisible(ok))
}
