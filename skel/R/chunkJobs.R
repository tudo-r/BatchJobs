#' Chunks ids of jobs together, so that each chunk will be executed as a single job.
#'
#' Useful for very short jobs or jobs of varying runtime.
#' Id vector is randomly permuted before it is being chunked.
#' You can pass the result of this function to submitJobs.
#
#' @param ids [\code{integer}]\cr
#'   Vector of job ids to be chunked.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}
#' @param n.chunks [\code{integer(1)}]\cr
#'   Preferred number of chunks.
#'   Can not be used in combination with \code{chunks.size}
#' @param shuffle [\code{logical(1)}]\cr
#'   Shuffle the vector of ids?
#'   Default is \code{TRUE}.
#' @return [list of \code{character}]. List of id vectors.
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' ids <- getJobIds(reg)
#' ch <- chunkJobs(ids, chunk.size=4)
#' submitJobs(reg, ch)
#' reduceResults(reg, fun=function(aggr, job, res) c(aggr, res))
chunkJobs = function(ids, chunk.size, n.chunks, shuffle=TRUE) {
  ids = convertIntegers(ids)
  checkArg(ids, "integer", na.ok=FALSE)
  chunk(ids, chunk.size, n.chunks, shuffle)
}
