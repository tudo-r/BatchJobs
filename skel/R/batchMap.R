#' Maps a function over a list adding jobs to a registry.
#' You can then submit these jobs to the batch system.
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param fun [\code{function}]\cr
#'   Function to map over \code{xs}.
#' @param ... [any]\cr
#'   Arguments to vectorize over (list or vector).
#' @param more.args [\code{list}]\cr
#'   A list of other arguments passed to \code{fun}.
#'   Default is empty list.
#' @return Nothing.
#' @examples \dontrun{
#' reg <- makeRegistry(id="BatchJobsExample", seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' print(reg)
#' }
#' @export
batchMap = function(reg, fun, ..., more.args=list()) {
  checkArg(reg, cl="Registry")
  checkArg(fun, cl="function")
  args = list(...)
  if(!all(vapply(args, is.vector, logical(1L))))
    stop("All args in '...' must be vectors!")
  n = unique(vapply(args, length, integer(1L)))
  if(length(n) != 1L)
    stop("All args in '...' must be of the same length!")
  if (getJobNr(reg) > 0L)
    stop("Registry is not empty!")
  seed = reg$seed
  jobs = lapply(seq_len(n), function(i) {
    ys = lapply(args, function(xs) xs[[i]])
    makeJob(fun=fun, pars=c(ys, more.args), seed=seed+i-1L)
  })
  addJobs(reg, jobs)
  invisible(NULL)
}
