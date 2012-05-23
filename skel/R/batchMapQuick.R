#' Combination of makeRegistry, batchMap and submitJobs.
#'
#' Combination of \code{\link{makeRegistry}}, \code{\link{batchMap}}
#' and \code{\link{submitJobs}}
#' for quick computations on the cluster.
#' Should only be used by skilled users who know what they are doing.
#' Creates the file.dir in current directory under the name \dQuote{.BatchJobs_bmq},
#' maps function, potentially chunks jobs and submits them.
#' @param fun [\code{function}]\cr
#'   Function to map over \code{...}.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @param packages [\code{character}]\cr
#'   Packages that will always be loaded on each node.
#'   Default is \code{character(0)}.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}
#'   Default is not to use chunking.
#' @param inds [\code{integer}]\cr
#'   Indices of ids / chunks to submit.
#'   Default is all.
#' @param resources [\code{list}]\cr
#'   Required resources for all batch jobs.
#'   Default is empty list.
#' @param temporary [\code{logical}]\cr
#'   Create the \code{file.dir} inside R's \code{tempdir}?
#'   Default is \code{FALSE}.
#' @return [\code{\link{Registry}}]
#' @export
batchMapQuick = function(fun, ..., more.args=list(), packages=character(0L),
  chunk.size, inds, resources=list(), temporary=FALSE) {
  checkArg(temporary, cl="logical", len=1L, na.ok=FALSE)
  id = basename(tempfile(pattern="bmq_"))
  fd = ifelse(temporary, file.path(tempdir(), id), id)
  reg = makeRegistry(id=id, file.dir=fd, packages=packages)
  # we want to return the reg in any case 
  # otherwise we cannot look at it / do anything with it in case of errors
  try({
    ids = batchMap(reg, fun, ..., more.args=more.args)
    if (!missing(chunk.size))
      ids = chunk(ids, chunk.size=chunk.size)
    if (missing(inds))
      inds = seq_along(ids)
    submitJobs(reg, ids[inds], resources=resources)
  }, silent=FALSE)
  return(reg)
}
