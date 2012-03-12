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
#' @examples \dontrun{
#'  # generating example results:
#'  result <- (1:10)^2
#'  
#'  # Define function to collect and reduce results:
#'  sq <- function(aggr, x) c(aggr,sqrt(x))
#'  # calculating the square roots, with 2 calculations per job:
#'  reg <- makeRegistry(id="BatchJobsExample", seed=123)
#'  batchReduce(reg, fun=sq, result, init=numeric(0), block.size=2)
#'  # starting 5 jobs
#'  submitJobs(reg)
#'  reduceResults(reg, fun=function(aggr,job,res) c(aggr, res))
#' }
#' @export
batchReduce = function(reg, fun, xs, init, block.size) {
  checkArg(reg, cl="Registry")
  checkArg(fun, formals=c("aggr", "x"))
  checkArg(xs, cl="vector")
  block.size = convertInteger(block.size)
  checkArg(block.size, "integer", len=1L, lower=1L, na.ok=FALSE)
  if (getJobNr(reg) > 0L)
    stop("Registry is not empty!")
  xs.blocks = chunk(xs, chunk.size = block.size)
  reduceOnSlave = function(xs.block, fun, init) {
    Reduce(fun, xs.block, init=init)
  }
  more.args = list(fun=fun, init=init)
  batchMap(reg, reduceOnSlave, xs.blocks, more.args=more.args)
  invisible(NULL)
}
