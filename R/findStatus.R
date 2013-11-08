findState = function(reg, ids, fun, negate) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun(reg, ids, negate)
}

#' Find jobs depending on computional state.
#'
#' \code{findDone}: Find jobs which succesfully terminated.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @return [\code{integer}]. Ids of jobs.
#' @export
#' @rdname findState
findDone = function(reg, ids) {
  findState(reg, ids, dbFindDone, FALSE)
}

#' \code{findNotDone}: Find jobs for which results are still missing.
#' @export
#' @rdname findState
findNotDone = function(reg, ids) {
  findState(reg, ids, dbFindDone, TRUE)
}

#' \code{findMissingResults}: Deprecated. Alias for findNotDone.
#' @export
#' @rdname findState
findMissingResults = function(reg, ids) {
  findNotDone(reg, ids)
}

#' \code{findErrors}: Find jobs where errors occured.
#' @export
#' @rdname findState
findErrors = function(reg, ids) {
  findState(reg, ids, dbFindErrors, FALSE)
}

#' \code{findNotErrors}: Find jobs where no errors occured.
#' @export
#' @rdname findState
findNotErrors = function(reg, ids) {
  findState(reg, ids, dbFindErrors, TRUE)
}

#' \code{findTerminated}: Find jobs which have terminated (done / error).
#' @export
#' @rdname findState
findTerminated = function(reg, ids) {
  findState(reg, ids, dbFindTerminated, FALSE)
}

#' \code{findNotTerminated}: Find jobs which have not terminated (not done / no error).
#' @export
#' @rdname findState
findNotTerminated = function(reg, ids) {
  findState(reg, ids, dbFindTerminated, TRUE)
}

#' \code{findSubmitted}: Find jobs which have been submitted.
#' @export
#' @rdname findState
findSubmitted = function(reg, ids) {
  findState(reg, ids, dbFindSubmitted, FALSE)
}

#' \code{findNotSubmitted}: Find jobs which have not been submitted.
#' @export
#' @rdname findState
findNotSubmitted = function(reg, ids) {
  findState(reg, ids, dbFindSubmitted, TRUE)
}


#' \code{findOnSystem}: Find jobs which are present on the batch system at the moment.
#' @export
#' @rdname findState
findOnSystem = function(reg, ids) {
  findState(reg, ids, dbFindOnSystem, FALSE)
}

#' \code{findNotOnSystem}: Find jobs which are not present on the batch system at the moment.
#' @export
#' @rdname findState
findNotOnSystem = function(reg, ids) {
  findState(reg, ids, dbFindOnSystem, TRUE)
}

#' \code{findRunning}: Find jobs which are running.
#' @export
#' @rdname findState
findRunning = function(reg, ids) {
  findState(reg, ids, dbFindRunning, FALSE)
}

#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findState
findNotRunning = function(reg, ids) {
  findState(reg, ids, dbFindRunning, TRUE)
}


#' \code{findStarted}: Find jobs which have been started on the batch system.
#' @export
#' @rdname findState
findStarted = function(reg, ids) {
  findState(reg, ids, dbFindStarted, FALSE)
}

#' \code{findStarted}: Find jobs which have not been started on the batch system.
#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findState
findNotStarted = function(reg, ids) {
  findState(reg, ids, dbFindStarted, TRUE)
}
#' \code{findExpired}: Find jobs where walltime was probably hit.
#' Right now the heuristic is as follows:
#' Find all jobs that have submitted, did not abort with an error,
#' did not complete with a result and are not submitted or running anymore.
#' @export
#' @rdname findState
findExpired = function(reg, ids) {
  findState(reg, ids, dbFindExpiredJobs, FALSE)
}
