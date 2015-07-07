findState = function(reg, ids, fun, negate, limit) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  if (!is.null(limit))
    assertCount(limit)
  fun(reg, ids, negate, limit)
}

#' Find jobs depending on computional state.
#'
#' \code{findDone}: Find jobs which succesfully terminated.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @param limit [\code{integer(1)}]\cr
#'   Limit the number of returned ids.
#'   Default is all ids.
#' @return [\code{integer}]. Ids of jobs.
#' @export
#' @rdname findState
findDone = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindDone, FALSE, limit)
}

#' \code{findNotDone}: Find jobs for which results are still missing.
#' @export
#' @rdname findState
findNotDone = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindDone, TRUE, limit)
}

#' \code{findMissingResults}: Deprecated. Alias for findNotDone.
#' @export
#' @rdname findState
findMissingResults = function(reg, ids, limit = NULL) {
  findNotDone(reg, ids, limit)
}

#' \code{findErrors}: Find jobs where errors occured.
#' @export
#' @rdname findState
findErrors = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindErrors, FALSE, limit)
}

#' \code{findNotErrors}: Find jobs where no errors occured.
#' @export
#' @rdname findState
findNotErrors = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindErrors, TRUE, limit)
}

#' \code{findTerminated}: Find jobs which have terminated (done / error).
#' @export
#' @rdname findState
findTerminated = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindTerminated, FALSE, limit)
}

#' \code{findNotTerminated}: Find jobs which have not terminated (not done / no error).
#' @export
#' @rdname findState
findNotTerminated = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindTerminated, TRUE, limit)
}

#' \code{findSubmitted}: Find jobs which have been submitted.
#' @export
#' @rdname findState
findSubmitted = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindSubmitted, FALSE, limit)
}

#' \code{findNotSubmitted}: Find jobs which have not been submitted.
#' @export
#' @rdname findState
findNotSubmitted = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindSubmitted, TRUE, limit)
}


#' \code{findOnSystem}: Find jobs which are present on the batch system at the moment.
#' @export
#' @rdname findState
findOnSystem = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindOnSystem, FALSE, limit)
}

#' \code{findNotOnSystem}: Find jobs which are not present on the batch system at the moment.
#' @export
#' @rdname findState
findNotOnSystem = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindOnSystem, TRUE, limit)
}

#' \code{findRunning}: Find jobs which are running.
#' @export
#' @rdname findState
findRunning = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindRunning, FALSE, limit)
}

#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findState
findNotRunning = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindRunning, TRUE, limit)
}


#' \code{findStarted}: Find jobs which have been started on the batch system.
#' @export
#' @rdname findState
findStarted = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindStarted, FALSE, limit)
}

#' \code{findStarted}: Find jobs which have not been started on the batch system.
#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findState
findNotStarted = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindStarted, TRUE, limit)
}

#' \code{findExpired}: Find jobs where walltime was probably hit.
#' Right now the heuristic is as follows:
#' Find all jobs that have started, did not abort with an error,
#' did not complete with a result and are not submitted or running anymore.
#' Note that this heuristic does not include jobs the scheduler looses before starting.
#' @export
#' @rdname findState
findExpired = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindExpiredJobs, FALSE, limit)
}

#' \code{findDisappeared}: Find jobs which disappeared from the system.
#' Right now the heuristic is as follows:
#' Find all jobs that are submitted but not started nor on the system anymore.
#' @export
#' @rdname findState
findDisappeared = function(reg, ids, limit = NULL) {
  findState(reg, ids, dbFindDisappeared, FALSE, limit)
}
