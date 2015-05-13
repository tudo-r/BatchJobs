#' Reset computational state of jobs.
#'
#' @description
#' Reset state of jobs in the database. Useful under two circumstances:
#' Either to re-submit them because of changes in e.g. external
#' data or to resolve rare issues when jobs are killed in an unfortunate state
#' and therefore blocking your registry.
#'
#' The function internally lists all jobs on the batch system and
#' if those include some of the jobs you want to reset, it informs you to kill them first by raising
#' an exception.
#' If you really know what you are doing, you may set \code{force} to \code{TRUE} to omit this sanity check.
#' Note that this is a dangerous operation to perform which may harm
#' the database integrity. In this case you HAVE to make externally sure that none of the jobs
#' you want to reset are still running.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs to kill.
#'   Default is none.
#' @param force [\code{logical(1)}]\cr
#'   Reset jobs without checking whether they are currently running.
#'   READ THE DETAILS SECTION!
#'   Default is \code{FALSE}.
#' @return Vector of reseted job ids.
#' @family debug
#' @export
resetJobs = function(reg, ids, force = FALSE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids) || length(ids) == 0L)
    return(integer(0L))
  ids = checkIds(reg, ids)
  assertFlag(force)

  if (!force) {
    if(is.null(getListJobs()) || is.null(getKillJob())) {
      stop("Listing or killing of jobs not supported by your cluster functions\n",
           "You need to set force = TRUE to reset jobs, but see the warning in ?resetJobs")
    }
    running = dbFindOnSystem(reg, ids, limit = 10L)
    if (length(running) > 0L)
      stopf("Can't reset jobs which are live on system. You have to kill them first!\nIds: %s",
            collapse(running))
  }

  info("Resetting %i jobs in DB.", length(ids))
  dbSendMessage(reg, dbMakeMessageKilled(reg, ids), staged = FALSE)
  invisible(ids)
}
