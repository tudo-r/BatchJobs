#' Reduce results from result directory.
#'
#' The following functions provide ways to reduce result files into either specific R objects (like
#' vectors, lists, matrices or data.frames) or to arbitrarily aggregate them, which is a more general
#' operation.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected jobs.
#'   Default is all jobs for which results are available.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param fun [\code{function}]\cr
#'   For \code{reduceResults}, a function \code{function(aggr, job, res, ...)} to reduce things,
#'   for all others, a function \code{function(job, res, ...)} to select stuff.
#'   Here, \code{job} is the current job descriptor (see \code{\link{Job}}), \code{result} is the current result object and
#'   \code{aggr} are the so far aggregated results. When using \code{reduceResults},
#'   your function should add the stuff you want to have from \code{job} and
#'   \code{result} to \code{aggr} and return that.
#'   When using the other reductions, you should select the stuff you want to have from \code{job} and
#'   \code{result} and return something that can be coerced to an element of the selected return data structure
#'   (reasonable conversion is tried internally).
#'   Default behavior for this argument is to return \code{res}, except for \code{reduceResults} where no
#'   default is available.
#' @param init [\code{ANY}]\cr
#'   Initial element, as used in \code{\link{Reduce}}.
#'   Default is first result.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should the return values be named?
#'   Default is \code{TRUE}.
#' @param rows [\code{logical(1)}]\cr
#'   Should the selected vectors be used as rows (or columns) in the result matrix?
#'   Default is \code{TRUE}.
#' @param strings.as.factors [\code{logical(1)}]
#'   Should all character columns in result be converted to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @return Aggregated results, return type depends on function. If \code{ids} is empty: \code{reduceResults}
#'   returns \code{init} (if available) or \code{NULL}, \code{reduceResultsVector} returns \code{c()},
#'   \code{reduceResultsList} returns \code{list()}, \code{reduceResultsMatrix} returns \code{matrix(0,0,0)},
#'   \code{reduceResultsDataFrame} returns \code{data.frame()}.
#' @export
#' @examples
#' # generate results:
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:5)
#' submitJobs(reg)
#'
#' # reduce results to a vector
#' reduceResultsVector(reg)
#' # reduce results to sum
#' reduceResults(reg, fun=function(aggr, job, res) aggr+res)
#'
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) list(a=x, b=as.character(2*x), c=x^2)
#' batchMap(reg, f, 1:5)
#' submitJobs(reg)
#' # reduce results to a vector
#' reduceResultsVector(reg, fun=function(job, res) res$a)
#' reduceResultsVector(reg, fun=function(job, res) res$b)
#' # reduce results to a list
#' reduceResultsList(reg)
#' # reduce results to a matrix
#' reduceResultsMatrix(reg, fun=function(job, res) res[c(1,3)])
#' reduceResultsMatrix(reg, fun=function(job, res) c(foo=res$a, bar=res$c), rows=TRUE)
#' reduceResultsMatrix(reg, fun=function(job, res) c(foo=res$a, bar=res$c), rows=FALSE)
#' # reduce results to a data.frame
#' print(str(reduceResultsDataFrame(reg)))
#' # reduce results to a sum
#' reduceResults(reg, fun=function(aggr, job, res) aggr+res$a, init=0)
reduceResults = function(reg, ids, part=NA_character_, fun, init, ...) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindDone(reg)
  } else {
    ids = checkIds(reg, ids)
    ndone = dbFindDone(reg, ids, negate=TRUE)
    if (length(ndone) > 0L)
      stopf("No results available for jobs with ids: %s", collapse(ndone))
  }
  checkArg(fun, formals=c("aggr", "job", "res"))

  n = length(ids)
  message("Reducing ", n, " results...")
  if (n == 0L) {
    if (missing(init))
      return(NULL)
    return(init)
  }

  bar = makeProgressBar(max=n, label="reduceResults")
  bar$set()

  tryCatch({
    if (missing(init)) {
      # fetch first result as init
      aggr = getResult(reg, ids[1L], part)
      ids = tail(ids, -1L)
      bar$inc(1L)
    } else {
      aggr = init
    }

    for (id in ids) {
      # use lazy evaluation:
      # If fun doesn't access job or res (unlikely), the
      # following statement is not executed. So, if the job variable
      # is not accessed, getJob will not trigger a database query
      aggr = fun(aggr,
                 job = getJob(reg, id, check.id=FALSE),
                 res = getResult(reg, id, part),
                 ...)
      bar$inc(1L)
    }
  }, error=bar$error)
  return(aggr)
}

reduceResultsReturnVal = function(reg, ids, part, fun, wrap, combine, use.names, name.fun, ..., init, empty) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids))
    ids = dbFindDone(reg)
  if (missing(fun)){
    fun = function(job, res) res
  } else {
    force(fun)
    checkArg(fun, formals=c("job", "res"))
  }
  n = length(ids)
  if (n == 0L) {
    message("Reducing ", n, " results...")
    return(empty)
  }
  fun2 = function(aggr, job, res, ...) combine(aggr, wrap(fun(job, res, ...)))
  res = reduceResults(reg, ids, part, fun2, init, ...)
  if (use.names)
    res = name.fun(res, ids, fun(getJob(reg, ids[1L]), getResult(reg, ids[1L], part)))
  return(res)
}


#' @export
#' @rdname reduceResults
reduceResultsVector = function(reg, ids, part=NA_character_, fun, ..., use.names=TRUE) {
  nf = function(res, ids, x1) {names(res) = ids; res}
  reduceResultsReturnVal(reg, ids, part, fun, identity, c, use.names, nf, ..., init=c(), empty=c())
}

#' @export
#' @rdname reduceResults
reduceResultsList = function(reg, ids, part=NA_character_, fun, ..., use.names=TRUE) {
  nf = function(res, ids, x1) {names(res) = ids; res}
  reduceResultsReturnVal(reg, ids, part, fun, list, c, use.names, nf, ..., init=list(), empty=list())
}

#' @export
#' @rdname reduceResults
reduceResultsMatrix = function(reg, ids, part=NA_character_, fun, ..., rows=TRUE, use.names=TRUE) {
  combine = if (rows) rbind else cbind
  if (rows)
    nf = function(res, ids, x1) {rownames(res) = ids; colnames(res) = names(x1); res}
  else
    nf = function(res, ids, x1) {colnames(res) = ids; rownames(res) = names(x1); res}
  res = reduceResultsReturnVal(reg, ids, part, fun, unlist, combine, use.names, nf, ..., init=c(), empty=matrix(0,0L,0L))
  if (!use.names)
    dimnames(res) = NULL
  return(res)
}

#' @export
#' @rdname reduceResults
reduceResultsDataFrame = function(reg, ids, part=NA_character_, fun, ...,
  strings.as.factors=default.stringsAsFactors()) {

  nf = function(res, ids, x1) {rownames(res) = ids; colnames(res) = names(x1); res}
  wrap = function(x) as.data.frame(x, stringsAsFactors=FALSE)
  res = reduceResultsReturnVal(reg, ids, part, fun, wrap, rbind, TRUE, nf, ..., init=data.frame(), empty=data.frame())
  stringsAsFactors(res, strings.as.factors)
}


