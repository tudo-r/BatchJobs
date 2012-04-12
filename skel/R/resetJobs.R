#' Reset a job on the batch system.
#'
#' Reset jobs in the database. Useful under two circumstances:
#' Either to re-submit them because of changes in e.g. external
#' data or to resolve rare issues whenjobs are killed in a unfortunate state 
#' and therefore blocking your registry.
#' Note that this is a dangerous operation to perform which may harm
#' the database integrity. Especially if reseted jobs are still running
#' this may corrupt your registry. If you are really know what you are
#' doing, you may set \code{force} to \code{TRUE} to omit sanity checks
#' on running jobs.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @param force [\code{integer}]\cr
#'   Also reset jobs which seem to be still running.
#'   Default is \code{FALSE}.
#' @return Nothing.
#' @export
resetJobs = function(reg, ids, force=FALSE) {
  checkArg(reg, cl="Registry")
  checkArg(force, cl="logical", len=1L, na.ok=FALSE)
  if (missing(ids))
    return(invisible(NULL))
  else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
  }
  if (!force) {
    if(is.null(getListJob()) || is.null(getKillJob())) {
      stop("Listing or killing of jobs not supported by your cluster functions\n",
           "You need to set force=TRUE to proceed resetting jobs, but note the warning in ?resetJobs")
    }
    running = findRunning(reg, ids)
    if (length(running) > 0L)
      stopf("Can't reset jobs which are still running: %s\nYou have to kill them first.", 
            collapse(running))
  }
  
  messagef("For safety reasons waiting 3secs...")
  Sys.sleep(3)
  messagef("Resetting %i jobs in DB.", length(ids))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids))
  invisible(NULL)
}
