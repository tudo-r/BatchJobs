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
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindDone(reg, ids)
}

#' \code{findNotDone}: Find jobs for which results are still missing.
#' @export
#' @rdname findState
findNotDone = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindDone(reg, ids, negate=TRUE)
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
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindErrors(reg, ids)
}

#' \code{findNotErrors}: Find jobs where no errors occured.
#' @export
#' @rdname findState
findNotErrors = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindErrors(reg, ids, negate=TRUE)
}

#' \code{findTerminated}: Find jobs which have terminated (done / error).
#' @export
#' @rdname findState
findTerminated = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindTerminated(reg, ids)
}

#' \code{findNotTerminated}: Find jobs which have not terminated (not done / no error).
#' @export
#' @rdname findState
findNotTerminated = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindTerminated(reg, ids, negate=TRUE)
}

#' \code{findSubmitted}: Find jobs which have been submitted.
#' @export
#' @rdname findState
findSubmitted = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindSubmitted(reg, ids)
}

#' \code{findNotSubmitted}: Find jobs which have not been submitted.
#' @export
#' @rdname findState
findNotSubmitted = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindSubmitted(reg, ids, negate=TRUE)
}


#' \code{findOnSystem}: Find jobs which are present on the batch system at the moment.
#' @export
#' @rdname findState
findOnSystem = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindOnSystem(reg, ids)
}

#' \code{findNotOnSystem}: Find jobs which are not present on the batch system at the moment.
#' @export
#' @rdname findState
findNotOnSystem = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindOnSystem(reg, ids, negate=TRUE)
}

#' \code{findRunning}: Find jobs which are running.
#' @export
#' @rdname findState
findRunning = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindRunning(reg, ids)
}

#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findState
findNotRunning = function(reg, ids) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindRunning(reg, ids, negate=TRUE)
}

#' \code{findExpired}: Find jobs where walltime was probably hit.
#' Right now the heuristic is as follows:
#' Find all jobs that have submitted, did not abort with an error,
#' did not complete with a result and are not submitted or running anymore.
#' @export
#' @rdname findState
findExpired = function(reg, ids) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  dbFindExpiredJobs(reg, ids)
}
