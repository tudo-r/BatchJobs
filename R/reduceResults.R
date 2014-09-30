#' Reduce results from result directory.
#'
#' @description
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
#' @param use.names [\code{character(1)}]\cr
#'   Name the results with job ids (\dQuote{ids}), stored job names (\dQuote{names})
#'   or return a unnamed result (\dQuote{none}).
#'   Default is \code{ids}.
#' @param impute.val [any]\cr
#'   For \code{reduceResults}: If not missing, the value of \code{impute.val} is passed to function \code{fun}
#'   as argument \code{res} for jobs with missing results.\cr
#'   For the specialized reduction functions \code{reduceResults[Type]}: If not missing, \code{impute.val} is
#'   used as a replacement for the return value of \code{fun} on missing results.
#' @template arg_progressbar
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
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) x^2
#' batchMap(reg, f, 1:5)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # reduce results to a vector
#' reduceResultsVector(reg)
#' # reduce results to sum
#' reduceResults(reg, fun = function(aggr, job, res) aggr+res)
#'
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) list(a = x, b = as.character(2*x), c = x^2)
#' batchMap(reg, f, 1:5)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # reduce results to a vector
#' reduceResultsVector(reg, fun = function(job, res) res$a)
#' reduceResultsVector(reg, fun = function(job, res) res$b)
#' # reduce results to a list
#' reduceResultsList(reg)
#' # reduce results to a matrix
#' reduceResultsMatrix(reg, fun = function(job, res) res[c(1,3)])
#' reduceResultsMatrix(reg, fun = function(job, res) c(foo = res$a, bar = res$c), rows = TRUE)
#' reduceResultsMatrix(reg, fun = function(job, res) c(foo = res$a, bar = res$c), rows = FALSE)
#' # reduce results to a data.frame
#' print(str(reduceResultsDataFrame(reg)))
#' # reduce results to a sum
#' reduceResults(reg, fun = function(aggr, job, res) aggr+res$a, init = 0)
reduceResults = function(reg, ids, part = NA_character_, fun, init, impute.val, progressbar = TRUE, ...) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = done = dbFindDone(reg)
    with.impute = FALSE
  } else {
    ids = checkIds(reg, ids)
    done = dbFindDone(reg, ids)
    with.impute = !missing(impute.val)
    if (!with.impute) {
      if (length(ids) > length(done))
        stopf("No results available for jobs with ids: %s", collapse(setdiff(ids, done)))
    }
  }
  assertFunction(fun, c("aggr", "job", "res"))
  assertFlag(progressbar)

  n = length(ids)
  info("Reducing ", n, " results...")
  if (n == 0L) {
    if (missing(init))
      return(NULL)
    return(init)
  }

  bar = getProgressBar(progressbar, max = n, label = "reduceResults")

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
                 res = if (with.impute && id %nin% done)
                   impute.val else getResult(reg, id, part),
                 ...)
      bar$inc(1L)
    }
  }, error = bar$error)
  return(aggr)
}

#' @export
#' @rdname reduceResults
reduceResultsList = function(reg, ids, part = NA_character_, fun, ..., use.names = "ids",
  impute.val, progressbar = TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = done = dbFindDone(reg)
    with.impute = FALSE
  } else {
    ids = checkIds(reg, ids)
    done = dbFindDone(reg, ids)
    with.impute = !missing(impute.val)
    if (!with.impute) {
      if (length(ids) > length(done))
        stopf("No results available for jobs with ids: %s", collapse(setdiff(ids, done)))
    }
  }
  if (missing(fun))
    fun = function(job, res) res
  else
    assertFunction(fun, c("job", "res"))
  use.names = convertUseNames(use.names)
  assertFlag(progressbar)

  n = length(ids)
  info("Reducing %i results...", n)
  if (n == 0L)
    return(list())
  res = vector("list", n)
  if (with.impute) {
    res = replace(res, ids %nin% done, impute.val)
    it = match(done, ids)
  } else {
    it = seq_len(n)
  }

  bar = getProgressBar(progressbar, max = n, label = "reduceResults")
  tryCatch({
    for (i in it) {
      # use lazy evaluation!
      tmp = fun(job = dbGetJobs(reg, ids[i])[[1L]], res = getResult(reg, ids[i], part), ...)
      if (!is.null(tmp))
        res[[i]] = tmp
      bar$inc(1L)
    }
  }, error = bar$error)

  names(res) = switch(use.names,
    "none" = NULL,
    "ids" = as.character(ids),
    "names" = dbGetJobNames(reg, ids))
  return(res)
}


#' @export
#' @rdname reduceResults
reduceResultsVector = function(reg, ids, part = NA_character_, fun, ..., use.names = "ids", impute.val) {
  unlist(reduceResultsList(reg, ids, part, fun, ..., use.names = use.names, impute.val = impute.val))
}

#' @export
#' @rdname reduceResults
reduceResultsMatrix = function(reg, ids, part = NA_character_, fun, ..., rows = TRUE, use.names = "ids", impute.val) {
  assertFlag(rows)
  use.names = convertUseNames(use.names)
  res = reduceResultsList(reg, ids, part, fun, ..., use.names = use.names, impute.val = impute.val)

  if (length(res) == 0L)
    return(matrix(0, nrow = 0L, ncol = 0L))

  n = length(res)
  dn = if (use.names != "none") list(names(res), names(res[[1L]])) else NULL
  res = unlist(res, use.names = FALSE)

  if (rows)
    matrix(res, nrow = n, byrow = TRUE, dimnames = dn)
  else
    matrix(res, ncol = n, byrow = FALSE, dimnames = rev(dn))
}

#' @export
#' @rdname reduceResults
reduceResultsDataFrame = function(reg, ids, part = NA_character_, fun, ..., use.names = "ids", impute.val,
  strings.as.factors = default.stringsAsFactors()) {
  assertFlag(strings.as.factors)

  res = reduceResultsList(reg, ids, part, fun, ..., use.names = use.names, impute.val = impute.val)
  if (!length(res))
    return(data.frame())

  convertListOfRowsToDataFrame(res, strings.as.factors = strings.as.factors)
}
