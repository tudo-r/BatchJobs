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
#' @param part [\code{character(1)}]\cr
#'   Only useful for multiple result files, then defines which result file part should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return Nothing.
#' @export
#' @examples
#' reg1 <- makeRegistry(id="BatchJobsExample1", file.dir=tempfile(), seed=123)
#' # square some numbers
#' f <- function(x) x^2
#' batchMap(reg1, f, 1:10)
#' submitJobs(reg1)
#' # look at results
#' reduceResults(reg1, fun=function(aggr,job,res) c(aggr, res))
#'  
#' reg2 <- makeRegistry(id="BatchJobsExample2", file.dir=tempfile(), seed=123)
#' # define function to tranform results, we simply do the inverse of the squaring
#' g <- function(job, res) sqrt(res)
#' batchMapResults(reg1, reg2, fun=g)
#' submitJobs(reg2)
#' # check results
#' reduceResults(reg2, fun=function(aggr,job,res) c(aggr, res))
batchMapResults = function(reg, reg2, fun, ...,  ids, part=as.character(NA), more.args=list()) {
  checkArg(reg, cl="Registry")
  checkArg(reg2, cl="Registry")
  if (missing(ids)) {
    ids = dbGetJobIdsIfAllDone(reg)
  } else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
    if (!all(ids %in% dbGetDone(reg)))
      stop("Not all jobs with corresponding ids finished (yet)!")
  }
  checkArg(fun, formals=c("job", "res"))
  if (getJobNr(reg2) > 0L)
    stop("Registry 'reg2' is not empty!")
  if(reg$file.dir == reg2$file.dir)
    stop("Both registries cannot point to the same file dir. Files would get overwritten!")
  fun2 = function(id, ...) {
    fun(job = getJob(reg, id, check.id=FALSE), 
        res = loadResult(reg, id, part=part, check.id=FALSE), 
        ...)
  }
  batchMap(reg2, fun2, ids, ..., more.args=more.args)
}
