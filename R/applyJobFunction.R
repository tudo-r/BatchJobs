#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param job [\code{\link{Job}}]\cr
#'   Job.
#' @return [any]. Result of job.
#' @keywords internal
#' @export
applyJobFunction = function(reg, job) {
  UseMethod("applyJobFunction")
}

#' @method applyJobFunction Registry
#' @S3method applyJobFunction Registry
applyJobFunction.Registry = function(reg, job) {
  do.call(job$fun, c(job$pars, job$more.args))
}
