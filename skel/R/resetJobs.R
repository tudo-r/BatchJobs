#' Reset computational state of jobs.
#'
#' Reset state of jobs in the database. Useful under two circumstances:
#' Either to re-submit them because of changes in e.g. external
#' data or to resolve rare issues when jobs are killed in an unfortunate state
#' and therefore blocking your registry.
#' Note that this is a dangerous operation to perform which may harm
#' the database integrity. You HAVE to externally make sure that none of the jobs
#' you want to reset are still running.
#' If you really know what you are
#' doing, you may set \code{force} to \code{TRUE} to omit sanity checks
#' on running jobs.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @param force [\code{logical(1)}]\cr
#'   Also reset jobs which seem to be still running.
#'   Default is \code{FALSE}.
#' @return Vector of reseted job ids.
#' @export
resetJobs = function(reg, ids, force=FALSE) {
  checkArg(reg, cl="Registry")
  if (missing(ids) || length(ids) == 0L)
    return(integer(0L))
  ids = checkIds(reg, ids)
  checkArg(force, cl="logical", len=1L, na.ok=FALSE)

  if (!force) {
    if(is.null(getListJobs()) || is.null(getKillJob())) {
      stop("Listing or killing of jobs not supported by your cluster functions\n",
           "You need to set force=TRUE to reset jobs, but note the warning in ?resetJobs")
    }
    running = findRunning(reg, ids)
    if (length(running) > 0L)
      stopf("Can't reset jobs which are still running. You have to kill them first.\nRunning: %s",
            collapse(running))
  }

  messagef("For safety reasons waiting 3secs...")
  Sys.sleep(3)
  messagef("Resetting %i jobs in DB.", length(ids))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids))
  invisible(ids)
}
