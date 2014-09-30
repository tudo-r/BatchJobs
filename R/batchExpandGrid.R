#' @title Map function over all combinations.
#'
#' @description
#' Maps an n-ary-function over a list of all combinations which are given by some vectors.
#' Internally \code{\link{expand.grid}} is used to compute the combinations, then
#' \code{\link{batchMap}} is called.
#'
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param fun [\code{function}]\cr
#'   Function to map over the combinations.
#' @param ... [any]\cr
#'   Vectors that are used to compute all combinations.
#'   If the arguments are named, these names are used to bind to arguments of \code{fun}.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return [\code{data.frame}]. Expanded grid of combinations produced by \code{\link{expand.grid}}.
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x, y, z) x * y  + z
#' # lets store the param grid
#' grid = batchExpandGrid(reg, f, x = 1:2, y = 1:3, more.args = list(z = 10))
#' submitJobs(reg)
#' waitForJobs(reg)
#' y = reduceResultsVector(reg)
#' # later, we can always access the param grid like this
#' grid = getJobParamDf(reg)
#' cbind(grid, y = y)
batchExpandGrid = function(reg, fun, ..., more.args = list()) {
  checkRegistry(reg, strict = TRUE)
  assertFunction(fun)
  args = list(...)
  ns = names(args)
  if (length(args) == 0L)
    return(invisible(integer(0L)))
  if(!all(vlapply(args, is.vector)))
    stop("All args in '...' must be vectors!")
  checkMoreArgs(more.args)
  reserved = c("KEEP.OUT.ATTRS", "stringsAsFactors")
  if (any(reserved %in% ns))
    stopf("You cannot use the reserved arg names %s in ... args!", collapse(reserved))
  args$KEEP.OUT.ATTRS = FALSE
  args$stringsAsFactors = FALSE

  grid = do.call(expand.grid, args)
  if (is.null(ns))
    colnames(grid) = NULL
  do.call(batchMap, c(as.list(grid), list(reg = reg, fun = fun, more.args = more.args)))
  return(setRowNames(grid, as.character(getJobIds(reg))))
}
