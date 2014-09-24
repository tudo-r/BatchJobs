#' Create a SubmitJobResult object.
#'
#' @description
#' Use this function in your implementation of \code{\link{makeClusterFunctions}}
#' to create a return value for the \code{submitJob} function.
#' @param status [\code{integer(1)}]\cr
#'   Launch status of job.
#'   0 means success, codes bewteen 1 and 100 are temporary
#'   errors and any error greater than 100 is a permanent failure.
#' @param batch.job.id [\code{character(1)}]\cr
#'   Unique id of this job on batch system. Note that this is not the usual job id used in BatchJobs!
#'   Must be globally unique so that the job can be terminated
#'   using just this information.
#' @param msg [\code{character(1)}]\cr
#'   Optional error message in case \code{status} is not equal to 0.
#'   Default is \dQuote{OK}, \dQuote{TEMPERR}, \dQuote{ERROR}, depending on \code{status}.
#' @param ... [\code{any}]\cr
#'   Currently unused.
#' @return [\code{\link{SubmitJobResult}}]. A list, containing
#'   \code{status}, \code{batch.job.id} and \code{msg}.
#' @export
#' @aliases SubmitJobResult
makeSubmitJobResult = function(status, batch.job.id, msg,  ...) {
  if (missing(msg)) {
    msg = if (status == 0L)
      "OK"
    else if (status <= 100L)
      "TEMPERR"
    else
      "ERROR"
  }
  setClasses(list(status = status, batch.job.id = batch.job.id, msg = msg), "SubmitJobResult")
}

#' @export
#' @method print SubmitJobResult
print.SubmitJobResult = function(x, ...) {
  cat("Job submission result:\n")
  catf("  ID     : '%s'", x$batch.job.id)
  catf("  Status : %i", x$status)
  catf("  Msg    : %s", x$msg)
}

#' Create a ClusterFuntions object.
#'
#' Use this funtion when you implement a backend for a batch system.
#' You must define the functions specified in the arguments.
#' @param name [\code{character(1)}]\cr
#'   Name of cluster functions.
#' @param submitJob [\code{function(conf, reg, job.name, rscript, log.file, job.dir, resources, ...)}]\cr
#'   Function to submit a new job.
#'   Must return a \code{\link{SubmitJobResult}} object.\cr
#'   The arguments are:\cr
#'   conf [\code{environment}]: The user configuration.\cr
#'   reg [\code{\link{Registry}}]: The registry.\cr
#'   job.name [\code{character(1)}]: Name of job, used if the job is displayed on the batch system. This is just for display and not an id!\cr
#'   rscript [\code{character(1)}]: File path to R script that is used to execute the job.\cr
#'   log.file [\code{character(1)}]: File path where log file (.Rout) has to be placed.\cr
#'   job.dir [\code{character(1)}]: Directory where all files relating to this job are placed.\cr
#'   resources [\code{list}]: Freely definable list of required resources for this job, e.g. walltime or memory.
#' @param killJob [\code{function(conf, reg, batch.job.id)}]\cr
#'   Function to kill a job on the batch system.
#'   Make sure that you definately kill the job!
#'   Return value is currently ignored.\cr
#'   The arguments are:\cr
#'   conf [\code{environment}]: The user configuration.\cr
#'   reg [\code{\link{Registry}}]: The registry.\cr
#'   batch.job.id [\code{character(1)}]: Batch job id, as produced by \code{submitJob}.\cr
#'   Set \code{killJob} to \code{NULL} if killing jobs cannot be supported.
#' @param listJobs [\code{function(conf, reg)}]\cr
#'   List all jobs on the batch system for the current user / registry.
#'   This includes queued, running, held, idle, etc. jobs.
#'   Must return an integer vector of batch job ids, same format as they are produced by \code{submitJob}.
#'   It does not matter if you return a few job ids too many (e.g. all for the current user instead
#'   of all for the current registry), but you have to include all relevant ones.
#'   The arguments are:\cr
#'   conf [\code{environment}]: The user configuration.\cr
#'   reg [\code{\link{Registry}}]: The registry.\cr
#'   Set \code{listJobs} to \code{NULL} if listing jobs cannot be supported.
#' @param getArrayEnvirName [\code{function()}]\cr
#'   Returns the name of the environment variable specifying the array ID.
#'   Should return \code{NA} if not supported.
#' @param class [\code{character(1)}]\cr
#'   Optional class name for cluster functions object.
#'   Useful to provide a nice print method
#'   which might show additional information about the workers.
#'   Default is \code{NULL}.
#' @param ... [\code{any}]\cr
#'   Currently ignored.
#' @export
#' @aliases ClusterFunctions
#' @family clusterFunctions
makeClusterFunctions = function(name, submitJob, killJob, listJobs, getArrayEnvirName, class = NULL, ...) {
  assertString(name)
  assertFunction(submitJob, c("conf", "reg", "job.name", "rscript", "log.file", "job.dir", "resources"))
  if (!is.null(killJob))
    assertFunction(killJob, c("conf", "reg", "batch.job.id"))
  if (!is.null(listJobs))
    assertFunction(listJobs, c("conf", "reg"))
  if (!is.null(getArrayEnvirName))
    assertFunction(getArrayEnvirName, character(0L))
  setClasses(list(name = name, submitJob = submitJob, killJob = killJob, listJobs = listJobs, getArrayEnvirName = getArrayEnvirName),
             c("ClusterFunctions", class))
}

#' @export
#' @method print ClusterFunctions
print.ClusterFunctions = function(x, ...) {
  catf("%s cluster functions.", x$name)
}
