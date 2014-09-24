#' Combination of makeRegistry, batchMap and submitJobs.
#'
#' @description
#' Combination of \code{\link{makeRegistry}}, \code{\link{batchMap}}
#' and \code{\link{submitJobs}}
#' for quick computations on the cluster.
#' Should only be used by skilled users who know what they are doing.
#' Creates the file.dir, maps function, potentially chunks jobs and submits them.
#'
#' @param fun [\code{function}]\cr
#'   Function to map over \code{...}.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @param file.dir [\code{character}]\cr
#'   See \code{\link{makeRegistry}}.
#'   Default is \code{NULL}, which means that it is created in the current directory under the name
#'   \dQuote{bmq_[random alphanumerics]}.
#' @param packages [\code{character}]\cr
#'   See \code{\link{makeRegistry}}.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}.
#'   Note that the ids will get shuffled to balance out possible run time differences.
#'   Default is not to use chunking.
#' @param n.chunks [\code{integer(1)}]\cr
#'   Preferred number chunks.
#'   Can not be used in combination with \code{chunk.size}.
#'   Note that the ids will get shuffled to balance out possible run time differences.
#'   Default is not to use chunking.
#' @param chunks.as.arrayjobs [\code{logical(1)}]\cr
#'   Submit chunks as array jobs?
#'   Default is \code{FALSE}.
#' @param inds [\code{integer}]\cr
#'   Indices of ids / chunks to submit.
#'   Default is all. If ids get chunked, this subsets the list of shuffled ids.
#' @param resources [\code{list}]\cr
#'   Required resources for all batch jobs.
#'   Default is empty list.
#' @return [\code{\link{Registry}}]
#' @export
batchMapQuick = function(fun, ..., more.args = list(), file.dir = NULL, packages = character(0L),
  chunk.size, n.chunks, chunks.as.arrayjobs = FALSE, inds, resources = list()) {
  if (is.null(file.dir)) {
    # create name for temp file dir in current dir
    file.dir = tempfile(pattern = "bmq_", tmpdir = getwd())
  } else {
    assertString(file.dir)
  }
  id = basename(file.dir)
  reg = makeRegistry(id = id, file.dir = file.dir, packages = packages)
  on.exit(messagef("Interrupted. You can find your registry in %s.", reg$file.dir))

  # we want to return the reg in any case
  # otherwise we cannot look at it / do anything with it in case of errors
  try({
    ids = batchMap(reg, fun, ..., more.args = more.args)
    if (!missing(chunk.size) || !missing(n.chunks))
      ids = chunk(ids, chunk.size = chunk.size, n.chunks = n.chunks, shuffle = TRUE)
    if (!missing(inds))
      ids = ids[inds]
    submitJobs(reg, ids, resources = resources)
  }, silent = FALSE)

  on.exit(NULL)
  return(reg)
}
