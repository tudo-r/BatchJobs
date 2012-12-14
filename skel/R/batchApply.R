#' A simple wrapper to batchMap to define jobs as an application of a function.
#'
#' on margins of matricies.
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param X [\code{\link{matrix}}]\cr
#'   A matrix.
#' @param margin [\code{integer(1)}]\cr
#'   Margin of the matrix. 1 for rows, 2 for columns.
#' @param fun [\code{function}]\cr
#'   Function to map over \code{...}.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}.
#'   Default is \code{chunk.size = 1} if \code{n.chunks} is also not provided:
#'   This results in \code{\link{nrow}} or \code{\link{ncol}} jobs, respectively.
#'   Note that the ids will get not shuffled.
#' @param n.chunks [\code{integer(1)}]\cr
#'   Preferred number chunks.
#'   Can not be used in combination with \code{chunk.size}.
#'   Note that the ids will get shuffled not shuffled.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @return Vector of type \code{integer} with job ids.
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' X = matrix(1:16, 4)
#' # Define two jobs to calculate the row sums:
#' batchApply(reg, X, 1, sum, n.chunks = 2)
#' submitJobs(reg)
#' reduceResultsVector(reg, use.names=FALSE) == rowSums(X)
#' @export
batchApply = function(reg, X, margin, fun, chunk.size, n.chunks, ...) {
  checkArg(X, cl="matrix")
  checkArg(fun, cl="function")
  margin = convertInteger(margin)
  checkArg(margin, "integer", len=1L, lower=1L, upper=2L, na.ok=FALSE)

  if (margin == 1L) {
    inds = seq_len(nrow(X))
    wrapper = function(.X, .inds, .user.fun, ...) {
      apply(.X[.inds,, drop=FALSE], 1L, .user.fun, ...)
    }
  } else {
    inds = seq_len(ncol(X))
    wrapper = function(.X, .inds, .user.fun, ...) {
      apply(.X[,.inds, drop=FALSE], 2L, .user.fun, ...)
    }
  }

  if (missing(chunk.size) && missing(n.chunks))
    chunk.size = 1L

  inds = chunk(inds, chunk.size=chunk.size, n.chunks=n.chunks, shuffle=FALSE)
  batchMap(reg, wrapper, .inds = inds, more.args = list(..., .X = X, .user.fun = fun))
}
