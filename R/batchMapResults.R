#' Maps a function over the results of a registry by using batchMap.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry whose results should be mapped by \code{fun}.
#' @param reg2 [\code{\link{Registry}}]\cr
#'   Empty registry that should store the job for the mapping.
#' @param fun [\code{function(job, res, ...)}]\cr
#'   Function to map over results of \code{reg}.
#'   Further arguments come from ... of \code{batchMapResults} and \code{more.args}.
#' @param ... [any]\cr
#'   Furher arguments to vectorize over (list or vector).
#'   Must all be the same length as number of results in \code{reg}.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs whose results should be mapped with \code{fun}.
#'   Default is all jobs.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return Vector of type \code{integer} with job ids.
#' @export
#' @examples
#' reg1 = makeRegistry(id = "BatchJobsExample1", file.dir = tempfile(), seed = 123)
#' # square some numbers
#' f = function(x) x^2
#' batchMap(reg1, f, 1:10)
#'
#' # submit jobs and wait for the jobs to finish
#' submitJobs(reg1)
#' waitForJobs(reg1)
#'
#' # look at results
#' reduceResults(reg1, fun = function(aggr,job,res) c(aggr, res))
#'
#' reg2 = makeRegistry(id = "BatchJobsExample2", file.dir = tempfile(), seed = 123)
#'
#' # define function to tranform results, we simply do the inverse of the squaring
#' g = function(job, res) sqrt(res)
#' batchMapResults(reg1, reg2, fun = g)
#'
#' # submit jobs and wait for the jobs to finish
#' submitJobs(reg2)
#' waitForJobs(reg2)
#'
#' # check results
#' reduceResults(reg2, fun = function(aggr,job,res) c(aggr, res))
batchMapResults = function(reg, reg2, fun, ...,  ids, part = NA_character_, more.args = list()) {
  # FIXME conserve jobnames
  checkRegistry(reg)
  syncRegistry(reg)
  checkRegistry(reg2)
  syncRegistry(reg2)
  assertFunction(fun, c("job", "res"))
  if (missing(ids)) {
    ids = dbGetJobIdsIfAllDone(reg)
  } else {
    ids = checkIds(reg, ids)
    if (length(dbFindDone(reg, ids, negate = TRUE, limit = 1L)) > 0L)
      stop("Not all jobs with corresponding ids finished (yet)!")
  }
  checkMoreArgs(more.args, reserved = c(".reg", ".fun", ".part"))
  if (dbGetJobCount(reg2) > 0L)
    stop("Registry 'reg2' is not empty!")
  if(reg$file.dir == reg2$file.dir)
    stop("Both registries cannot point to the same file dir. Files would get overwritten!")
  reg2$packages = insert(reg2$packages, reg$packages)
  saveRegistry(reg2)

  batchMap(reg2, batchMapResultsWrapper, ids, ..., more.args = c(more.args, list(.reg = reg, .fun = fun, .part = part)))
}

batchMapResultsWrapper = function(id, ..., .reg, .fun, .part) {
  .fun(job = getJob(.reg, id, check.id = FALSE),
    res = getResult(.reg, id, part = .part), ...)
}
