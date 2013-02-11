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
# FIXME we need more documentation for reduceResultsReturnValue ...
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
      # use lazy evaluation
      aggr = fun(aggr,
                 job = dbGetJobs(reg, id)[[1L]],
                 res = getResult(reg, id, part),
                 ...)
      bar$inc(1L)
    }
  }, error=bar$error)
  return(aggr)
}

#' @export
#' @rdname reduceResults
reduceResultsList = function(reg, ids, part=NA_character_, fun, ..., use.names = TRUE) {
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
  if (missing(fun))
    fun = function(job, res) res
  else
    checkArg(fun, formals=c("job", "res"))
  checkArg(use.names, "logical", len=1L, na.ok=FALSE)

  n = length(ids)
  message("Reducing ", n, " results...")
  if (n == 0L)
    return(list())
  res = vector("list", n)

  bar = makeProgressBar(max=n, label="reduceResults")
  bar$set()
  tryCatch({
    for (i in seq_along(ids)) {
      # use lazy evaluation!
      res[[i]] = fun(job = dbGetJobs(reg, ids[i])[[1L]],
                     res = getResult(reg, ids[i], part), ...)
      bar$inc(1L)
    }
  }, error=bar$error)

  if (use.names)
    names(res) = ids
  return(res)
}


#' @export
#' @rdname reduceResults
reduceResultsVector = function(reg, ids, part=NA_character_, fun, ..., use.names=TRUE) {
  unlist(reduceResultsList(reg, ids, part, fun, ..., use.names = use.names))
}

#' @export
#' @rdname reduceResults
reduceResultsMatrix = function(reg, ids, part=NA_character_, fun, ..., rows=TRUE, use.names=TRUE) {
  checkArg(rows, "logical", len=1L, na.ok=FALSE)
  res = reduceResultsList(reg, ids, part, fun, ..., use.names = use.names)

  if (length(res) == 0L)
    return(matrix(0, nrow = 0L, ncol = 0L))

  n = length(res)
  dn = if (use.names) list(names(res), names(res[[1L]])) else NULL
  res = unlist(res, use.names = FALSE)

  if (rows)
    matrix(res, nrow = n, byrow = TRUE, dimnames = dn)
  else
    matrix(res, ncol = n, byrow = FALSE, dimnames = rev(dn))
}

#' @export
#' @rdname reduceResults
reduceResultsDataFrame = function(reg, ids, part=NA_character_, fun, ..., use.names=TRUE,
  strings.as.factors=default.stringsAsFactors()) {
  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)

  res = reduceResultsList(reg, ids, part, fun, ..., use.names = use.names)
  if (length(res) == 0L)
    return(data.frame())

  list2df(res, force.names=TRUE, strings.as.factors = strings.as.factors)
}
