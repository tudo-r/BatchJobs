#' applyJobFunction
#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param job [\code{\link{Job}}]\cr
#'   Job.
#' @param cache [\code{FileCache}]\cr
#'   Instance of \code{\link[BBmisc]{makeFileCache}}.
#' @return [any]. Result of job.
#' @keywords internal
#' @export
applyJobFunction = function(reg, job, cache) {
  UseMethod("applyJobFunction")
}

#' @method applyJobFunction Registry
#' @export
applyJobFunction.Registry = function(reg, job, cache) {
  fn = file.path(getFunDir(reg$file.dir), sprintf("%s.RData", job$fun.id))
  stuff = cache(fn, parts = c("fun", "more.args"), simplify = FALSE)
  do.call(stuff$fun, c(job$pars, stuff$more.args))
}
