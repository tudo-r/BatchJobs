#' Maps a function over lists or vectors, adding jobs to a registry.
#'
#' You can then submit these jobs to the batch system.
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param fun [\code{function}]\cr
#'   Function to map over \code{...}.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @param jobnames [\code{character}]\cr
#'   Names for arguments. If missing, names are retrieved from the first
#'   argument in \dQuote{...} (either named or a character vector).
#' @return Vector of type \code{integer} with job ids.
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' print(reg)
#' @export
batchMap = function(reg, fun, ..., more.args=list(), jobnames) {
  checkRegistry(reg, strict=TRUE)
  checkArg(fun, cl="function")
  args = list(...)
  if (length(args) == 0L)
    return(invisible(integer(0L)))
  if(!all(vapply(args, is.vector, logical(1L))))
    stop("All args in '...' must be vectors!")
  n = unique(vapply(args, length, integer(1L)))
  if(length(n) != 1L)
    stop("All args in '...' must be of the same length!")
  if (n == 0L)
    return(invisible(integer(0L)))
  checkMoreArgs(more.args)
  jobnames = getArgNames(args, jobnames)
  if (is.null(jobnames))
    jobnames = rep(NA_character_, n)

  if (dbGetJobCount(reg) > 0L)
    stop("Registry is not empty!")
  messagef("Adding %i jobs to DB.", n)

  # create seeds
  seed = reg$seed
  seeds = addIntModulo(seed, seq(0L, n-1L))

  # serialize pars to char vector
  pars = mapply(function(...) {
    rawToChar(serialize(list(...), connection=NULL, ascii=TRUE))
  }, ..., USE.NAMES=FALSE)
  fun.id = saveFunction(reg, fun, more.args)

  # add jobs to DB
  n = dbAddData(reg, "job_def", data = data.frame(fun_id=fun.id, pars=pars, jobname=jobnames))
  job.def.ids = dbGetLastAddedIds(reg, "job_def", "job_def_id", n)
  n = dbAddData(reg, "job_status", data=data.frame(job_def_id=job.def.ids, seed=seeds))
  job.ids = dbGetLastAddedIds(reg, "job_status", "job_id", n)

  # we can only create the dir after we have obtained the ids from the DB
  createShardedDirs(reg, job.ids)
  invisible(job.ids)
}
