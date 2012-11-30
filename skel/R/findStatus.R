findPos = function(reg, ids, fun) {
  checkRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun(reg, ids)
}

findNeg = function(reg, ids, fun) {
  checkRegistry(reg)
  if (!missing(ids)) {
    ids2 = checkIds(reg, ids)
  } else  {
    ids2 = getJobIds(reg)
  }
  setdiff(ids2, fun(reg, ids))
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
findDone = function(reg, ids) {
  findPos(reg, ids, dbFindDone)
}

#' \code{findNotDone}: Find jobs for which results are still missing.
#' @export
#' @rdname findDone
findNotDone = function(reg, ids) {
  findNeg(reg, ids, dbFindDone)
}

#' \code{findErrors}: Find jobs where errors occured.
#' @export
#' @rdname findDone
findErrors = function(reg, ids) {
  findPos(reg, ids, dbFindErrors)
}

#' \code{findNotErrors}: Find jobs where no errors occured.
#' @export
#' @rdname findDone
findNotErrors = function(reg, ids) {
  findNeg(reg, ids, dbFindErrors)
}

#' \code{findTerminated}: Find jobs which have terminated (done / error).
#' @export
#' @rdname findDone
findTerminated = function(reg, ids) {
  findPos(reg, ids, dbFindTerminated)
}

#' \code{findNotTerminated}: Find jobs which have not terminated (not done / no error).
#' @export
#' @rdname findDone
findNotTerminated = function(reg, ids) {
  findNeg(reg, ids, dbFindTerminated)
}

#' \code{findSubmitted}: Find jobs which have been submitted.
#' @export
#' @rdname findDone
findSubmitted = function(reg, ids) {
  findPos(reg, ids, dbFindSubmitted)
}

#' \code{findNotSubmitted}: Find jobs which have not been submitted.
#' @export
#' @rdname findDone
findNotSubmitted = function(reg, ids) {
  findNeg(reg, ids, findSubmitted)
}


#' \code{findOnSystem}: Find jobs which are present on the batch system at the moment.
#' @export
#' @rdname findDone
findOnSystem = function(reg, ids) {
  findPos(reg, ids, dbFindOnSystem)
}

#' \code{findNotOnSystem}: Find jobs which are not present on the batch system at the moment.
#' @export
#' @rdname findDone
findNotOnSystem = function(reg, ids) {
  findNeg(reg, ids, findOnSystem)
}

#' \code{findRunning}: Find jobs which are running.
#' @export
#' @rdname findDone
findRunning = function(reg, ids) {
  findPos(reg, ids, dbFindRunning)
}

#' \code{findNotRunning}: Find jobs which are not running.
#' @export
#' @rdname findDone
findNotRunning = function(reg, ids) {
  findNeg(reg, ids, findRunning)
}

#' \code{findExpired}: Find jobs where walltime was probably hit.
#' Right now the heuristic is as follows:
#' Find all jobs that have submitted, did not abort with an error,
#' did not complete with a result and are not submitted or running anymore.
#' @export
#' @rdname findDone
findExpired = function(reg, ids) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  fun = getListJobs("Cannot find expired jobs")
  batch.job.ids = fun(getBatchJobsConf(), reg)
  dbFindExpiredJobs(reg, batch.job.ids, ids)
}
