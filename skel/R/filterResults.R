#' Find all results where a specific condition is true.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs whose results you want to test for the condition.
#'   Default is all jobs for which results are available.
#' @param fun [\code{fun(job, res)}]\cr
#'   Predicate function that returns \code{TRUE} or \code{FALSE}.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @return [\code{character}]. Ids of jobs where \code{fun(job, result)} returns \code{TRUE}.
#' @examples \dontrun{
#'  reg <- makeRegistry(id="BatchJobsExample", seed=123)
#'  f <- function(x) x^2
#'  batchMap(reg, f, 1:10)
#'  submitJobs(reg)
#' 
#'  # which square numbers are even:
#'  even <- function(job, res) res%%2==0
#'  filterResults(reg, fun=even)
#' }
#' @export
filterResults = function(reg, ids, fun, ...) {
  checkArg(reg, "Registry")
  checkArg(fun, formals=c("job", "res"))
  if (missing(ids))
    ids = dbGetDone(reg)
  else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    checkIds(reg, ids)
  }
  Filter(function(id) {
    fun(job = getJob(reg, id, check.id=FALSE), 
        res = loadResult(reg, id, check.id=FALSE), 
        ...)
  }, ids)
}
