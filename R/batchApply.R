# @title Apply a function on maticies or arrays
#
# A simple wrapper to batchMap to define jobs as an application of a function.
# on margins of matrices or arrays.
#
# @param reg [\code{\link{Registry}}]\cr
#   Empty Registry that will store jobs for the mapping.
# @param X [\code{\link[base]{matrix}} | \code{\link[base]{array}}]\cr
#   A matrix or an array.
# @param margin [\code{integer(1)}]\cr
#   Margin of the matrix. 1 for rows, 2 for columns.
# @param fun [\code{function}]\cr
#   Function to map over \code{...}.
# @param chunk.size [\code{integer(1)}]\cr
#   Preferred number of jobs in each chunk.
#   Can not be used in combination with \code{n.chunks}.
#   Default is \code{chunk.size = 1} if \code{n.chunks} is also not provided and
#   results in \code{\link{nrow}} or \code{\link{ncol}} jobs, respectively.
# @param n.chunks [\code{integer(1)}]\cr
#   Preferred number of chunks.
#   Can not be used in combination with \code{chunk.size}.
# @param ... [any]\cr
#   Additional arguments passed to \code{fun}.
# @return [\code{integer}]. Ids of created jobs.
# @examples
#  reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#  X = matrix(1:16, 4)
#  # Define two jobs to calculate the row sums:
#  batchApply(reg, X, 1, sum, n.chunks = 2)
#  submitJobs(reg)
#  waitForJobs(reg)
#  reduceResultsVector(reg, use.names = FALSE) == rowSums(X)
batchApply = function(reg, X, margin, fun, chunk.size, n.chunks, ..., use.names = FALSE) {
  assert(checkMatrix(X), checkArray(X))
  dX = dim(X)
  margin = asInt(margin, lower = 1L, upper = length(dX))
  fun = checkUserFunction(fun)
  more.args = list(...)
  checkMoreArgs(more.args, c(".X", ".inds", ".user.fun"))
  assertFlag(use.names)

  inds = chunk(seq_len(dX[margin]), chunk.size = chunk.size, n.chunks = n.chunks, shuffle = FALSE)
  if (use.names && !is.null(dimnames(X)[[margin]]))
    names(inds) = dimnames(X)[[margin]]

  more.args = c(more.args, list(.X = aperm(X, c(margin, seq_along(dX)[-margin])), .user.fun = fun))
  batchMap(reg, batchApplyWrapper, .inds = inds, more.args = more.args, use.names = use.names)
}

batchApplyWrapper = function(.inds, .X, .user.fun, ...) {
  apply(.X[.inds,, drop = FALSE], 1L, .user.fun, ...)
}

# FIXME: we should split the matrix on the master first.
# here is a piece of code which allows this, but there is no sufficient
# mechanism in BJ to find these files again on the slave?
# maybe store in work.dir?
# splitX = function(X, margin, chunks) {
#   n = length(chunks)
#   X = aperm(X, c(margin, seq_along(dim(X))[-margin]))
#   dn = file.path(reg$file.dir, "data-chunks")
#   dir = checkDir(dn, create = TRUE)
#   dest = file.path(dn, sprintf("chunk-%i.RData", seq_len(n)))
#
#   info("Splitting input into %i files ...", n)
#   for (i in seq_len(n)) {
#     save2(.X = X[chunks[[i]],, drop = FALSE], file = dest[i])
#   }
#   dest
# }
