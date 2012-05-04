#' Finds ids of jobs that match a query.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
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
#' # the next line looks a bit clumsy, we need .arg1 because batchExpandGrid calls batchMap internally
#' findJobs(reg, pars=quote(.arg1$y > 2))


findJobs = function(reg, pars) {
  checkArg(reg, cl="Registry")
  jobs = getJobs(reg, check.ids=FALSE)
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
  if (!is.evaluable(pars))
    stop("Argument 'pars' must be a call, expression or symbol!")
  jobs = Filter(function(j) eval(pars, rename(j$pars)), jobs)
  return(extractSubList(jobs, "id", element.value=integer(1L)))
}

