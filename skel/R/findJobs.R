#' Finds ids of jobs that match a query.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Subset of job ids to restrict the result to.
#'   Default is all jobs.
#' @param pars [quoted R expression]\cr
#'   All jobs whose parameters match the given expression are selected.
#'   This implies that you have named the parameters when you passed the vectors.
#'   If you forgot to do this you can use \code{.arg1}, \code{.arg2}, etc., to refer to the
#'   the unnamed ones.
#' @return [\code{integer}]. Ids for jobs which match the query.
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x, y) x * y
#' batchExpandGrid(reg, f, x=1:2, y=1:3)
#' findJobs(reg, pars=quote(y > 2))
findJobs = function(reg, ids, pars) {
  checkArg(reg, cl="Registry")
  if (!missing(ids))
    ids = checkIds(reg, ids)
  if (!is.evaluable(pars))
    stop("Argument 'pars' must be a call, expression or symbol!")
  jobs = getJobs(reg, ids, check.ids=FALSE)

  rename = function(pars) {
    ns = names(pars)
    if (is.null(ns)) {
      ns = rep("", length(pars))
    }
    j = which(is.na(ns) | ns == "")
    ns[j] = paste(".arg", seq_along(j), sep="")
    names(pars) = ns
    pars
  }

  jobs = Filter(function(j) eval(pars, rename(j$pars)), jobs)
  return(extractSubList(jobs, "id", element.value=integer(1L)))
}
