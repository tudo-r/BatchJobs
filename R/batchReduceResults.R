#' Reduces results via a binary function and adds jobs for this to a registry.
#'
#' @description
#' Each jobs reduces a certain number of results on one slave.
#' You can then submit these jobs to the batch system.
#' Later, you can do a final reduction with \code{\link{reduceResults}} on the master.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry whose results should be reduced by \code{fun}.
#' @param reg2 [\code{\link{Registry}}]\cr
#'   Empty registry that should store the job for the mapping.
#' @param fun [\code{function(aggr, job, res, ...)}]\cr
#'   Function to reduce results with.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs whose results should be reduced with \code{fun}.
#'   Default is all jobs.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param init [any]\cr
#'   Initial object for reducing.
#' @param block.size [\code{integer(1)}]\cr
#'   Number of results reduced in one job.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return Vector of type \code{integer} with job ids.
#' @export
#' @examples
#' # generating example results:
#' reg1 = makeRegistry(id = "BatchJobsExample1", file.dir = tempfile(), seed = 123)
#' f = function(x) x^2
#' batchMap(reg1, f, 1:20)
#' submitJobs(reg1)
#' waitForJobs(reg1)
#'
#' # define function to reduce on slave, we want to sum the squares
#' myreduce = function(aggr, job, res) aggr + res
#'
#' # sum 5 results on each slave process, i.e. 4 jobs
#' reg2 = makeRegistry(id = "BatchJobsExample2", file.dir = tempfile(), seed = 123)
#' batchReduceResults(reg1, reg2, fun = myreduce, init = 0, block.size = 5)
#' submitJobs(reg2)
#' waitForJobs(reg2)
#'
#' # now reduce one final time on master
#' reduceResults(reg2, fun = myreduce)
batchReduceResults = function(reg, reg2, fun, ids, part = NA_character_, init, block.size, more.args = list()) {
  checkRegistry(reg)
  checkRegistry(reg2)
  syncRegistry(reg)
  syncRegistry(reg2)
  assertFunction(fun, c("aggr", "job", "res"))
  if (missing(ids)) {
    ids = dbGetJobIdsIfAllDone(reg)
  } else {
    ids = checkIds(reg, ids)
    if (length(dbFindDone(reg, ids, negate = TRUE, limit = 1L)) > 0L)
      stop("Not all jobs with corresponding ids finished (yet)!")
  }
  block.size = asCount(block.size)
  checkMoreArgs(more.args, reserved = c("..reg", "..fun", "..part"))

  if (dbGetJobCount(reg2) > 0L)
    stop("Registry 'reg2' is not empty!")
  if(reg$file.dir == reg2$file.dir)
    stop("Both registries cannot point to the same file dir. Files would get overwritten!")
  reg2$packages = insert(reg2$packages, reg$packages)
  saveRegistry(reg2)

  batchReduce(reg2, batchReduceResultsWrapper, ids, init = init, block.size = block.size,
    more.args = c(more.args, list(..reg = reg, ..fun = fun, ..part = part)))
}

batchReduceResultsWrapper = function(aggr, x, ..reg, ..fun, ..part) {
  # x is id
  # use lazy evaluation, if fun doesn't access job or res (unlikely)
  ..fun(aggr = aggr, job = getJob(..reg, x, check.id = FALSE),
        res = getResult(..reg, x, ..part))
}
