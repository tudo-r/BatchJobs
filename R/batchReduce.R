#' Reduces via a binary function over a list adding jobs to a registry.
#'
#' @description
#' Each jobs reduces a certain number of elements on one slave.
#' You can then submit these jobs to the batch system.
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry.
#' @param fun [\code{function(aggr, x, ...)}]\cr
#'   Function to reduce \code{xs} with.
#' @param xs [\code{vector}]\cr
#'   Vector to reduce.
#' @param init [any]\cr
#'   Initial object for reducing.
#' @param block.size [\code{integer(1)}]\cr
#'   Number of elements of \code{xs} reduced in one job.
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return Vector of type \code{integer} with job ids.
#' @export
#' @examples
#' # define function to reduce on slave, we want to sum a vector
#' f = function(aggr, x) aggr + x
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#'
#' # sum 20 numbers on each slave process, i.e. 5 jobs
#' batchReduce(reg, fun = f, 1:100, init = 0, block.size = 5)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # now reduce one final time on master
#' reduceResults(reg, fun = function(aggr,job,res) f(aggr, res))
batchReduce = function(reg, fun, xs, init, block.size, more.args = list()) {
  checkRegistry(reg, strict = TRUE)
  syncRegistry(reg)
  assertFunction(fun, c("aggr", "x"))
  if (!is.vector(xs))
    stop("Argument xs must be a vector")
  block.size = asCount(block.size, positive = TRUE)
  if (dbGetJobCount(reg) > 0L)
    stop("Registry is not empty!")
  checkMoreArgs(more.args, reserved = c(".fun", ".init"))
  xs.blocks = chunk(xs, chunk.size = block.size, shuffle = FALSE)
  more.args = c(more.args, list(.fun = fun, .init = init))
  batchMap(reg, batchReduceWrapper, xs.blocks, more.args = more.args)
}

batchReduceWrapper = function(xs.block, .fun, .init, ...) {
  fun = function(aggr, x)
    .fun(aggr, x, ...)
  Reduce(fun, xs.block, init = .init)
}
