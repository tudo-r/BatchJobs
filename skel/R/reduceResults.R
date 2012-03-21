#' Reduce results from result directory into a single R object, e.g. a data.frame.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected jobs.
#'   Default is all jobs for which results are available.
#' @param part [\code{character(1)}]\cr
#'   Only useful for multiple result files, then defines which result file part should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param fun [\code{function(aggr, job, res)}]\cr
#'   Function used to reduce results.
#'   \code{aggr} are the so far aggregated results, \code{job} is the current
#'   job descriptor, \code{result} is the current result object.
#'   Your function should now add the stuff you want to have from \code{job} and
#'   \code{result} to \code{aggr} and return that.
#' @param init [\code{ANY}]\cr
#'   Initial element, as used in \code{\link{Reduce}}.
#'   Default is first result.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @return [any]. Aggregated results.
#' @export
#' @examples
#' # generate results:
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
#' 
#' # reduce results to a vector
#' reduceResults(reg, fun=function(aggr, job, res) c(aggr, res))
reduceResults = function(reg, ids, part=as.character(NA), fun, init, ...) {
  checkArg(reg, "Registry")
  checkArg(fun, formals=c("aggr", "job", "res"))

  if (missing(ids)) {
    ids = dbGetDone(reg)
  } else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
  }

  n = length(ids)
  if (n == 0L)
    stop("No jobs with corresponding ids finished (yet)!")

  message("Reducing ", n, " results...")
  bar = makeProgressBar(max=n, label="reduceResults")
  bar(0L)

  if (missing(init)) {
    # fetch first result as init
    aggr = loadResult(reg, ids[1L], part, check.id=FALSE)
    bar(1L)
    if (n == 1L)
      return(aggr)
  } else {
    aggr = init
  }
  
  for (i in seq(from = 1L + missing(init), to = length(ids))) {
    id = ids[i]
    # use lazy evaluation:
    # If fun doesn't access job or res (unlikely), the
    # following statement is not executed. So, if the job variable
    # is not accessed, getJob will not trigger a database query
    aggr = fun(aggr,
               job = getJob(reg, id, check.id=FALSE), 
               res = loadResult(reg, id, part, check.id=FALSE), 
               ...)
    bar(i)
  }
  return(aggr)
}
