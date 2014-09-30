#' Find all results where a specific condition is true.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs whose results you want to test for the condition.
#'   Default is all jobs for which results are available.
#' @param fun [\code{fun(job, res)}]\cr
#'   Predicate function that returns \code{TRUE} or \code{FALSE}.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @return [\code{integer}]. Ids of jobs where \code{fun(job, result)} returns \code{TRUE}.
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # which square numbers are even:
#' filterResults(reg, fun = function(job, res) res %% 2 == 0)
filterResults = function(reg, ids, fun, ...) {
  checkRegistry(reg)
  syncRegistry(reg)
  assertFunction(fun, c("job", "res"))
  if (missing(ids))
    ids = dbFindDone(reg)
  else
    ids = checkIds(reg, ids)

  Filter(function(id) {
    fun(job = getJob(reg, id, check.id = FALSE),
        res = getResult(reg, id),
        ...)
  }, ids)
}
