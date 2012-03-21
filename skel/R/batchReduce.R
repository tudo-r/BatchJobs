#' Reduces via a binary function over a list adding jobs to a registry.
#' Each jobs reduces a certain number of elements on one slave.
#' You can then submit these jobs to the batch system.
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry.
#' @param fun [\code{function(aggr, x)}]\cr
#'   Function to reduce \code{xs} with.
#' @param xs [\code{vector}]\cr
#'   Vector to reduce.
#' @param init [any]\cr
#'   Initial object for reducing.
#' @param block.size [\code{integer(1)}]\cr
#'   Number of elements of \code{xs} reduced in one job.
#' @return Nothing.
#' @export
#' @examples
#' # define function to reduce on slave, we want to sum a vector
#' f <- function(aggr, x) aggr + x
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' # sum 20 numbers on each slave process, i.e. 5 jobs
#' batchReduce(reg, fun=f, 1:100, init=0, block.size=5)
#' submitJobs(reg)
#' # now reduce one final time on master
#' reduceResults(reg, fun=function(aggr,job,res) f(aggr, res))
batchReduce = function(reg, fun, xs, init, block.size) {
  checkArg(reg, cl="Registry")
  checkArg(fun, formals=c("aggr", "x"))
  checkArg(xs, cl="vector")
  block.size = convertInteger(block.size)
  checkArg(block.size, "integer", len=1L, lower=1L, na.ok=FALSE)
  if (getJobNr(reg) > 0L)
    stop("Registry is not empty!")
  xs.blocks = chunk(xs, chunk.size = block.size, shuffle=FALSE)
  reduceOnSlave = function(xs.block, fun, init) {
    Reduce(fun, xs.block, init=init)
  }
  more.args = list(fun=fun, init=init)
  batchMap(reg, reduceOnSlave, xs.blocks, more.args=more.args)
  invisible(NULL)
}
