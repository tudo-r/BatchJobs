#' Submits a single function call as a single job.
#'
#' Convenience function if you have just one lengthy job to compute on the cluster.
#' Registry is created internally in temporary directory.
#' The id of the auto-created job is always 1.
#' The result is stored in a file of your choice.
#' For details on retries look at \code{\link{submitJobs}}.
#'
#' @param id [\code{integer(1)}]\cr
#'   Id of registry, which is created internally to submit the single job.
#' @param fun [\code{function}]\cr
#'   Function to apply to \code{pars}.
#' @param pars [\code{list}]\cr
#'   A list of arguments passed to \code{fun}.
#' @param file [\code{character(1)}]\cr
#'   Path to file where result should be saved.
#' @param seed [\code{integer(1)}]\cr
#'   Seed for function call.
#'   Default is a random number from 1 to \code{.Machine$integer.max/2}.
#' @param resources [\code{list}]\cr
#'   Required resources for the batch job.
#'   Default is empty list.
#' @param wait [\code{function(retries)}]\cr
#'   Function that defines how many seconds should be waited in case of a temporary error.
#'   Default is exponential back-off with \code{50*2^retries}.
#' @param max.retries [\code{integer(1)}]\cr
#'   Number of times to submit one job again in case of a temporary error
#'   (like filled queues). Each time \code{wait} is called to wait a certain
#'   number of seconds.
#'   Default is 10 times.
#' @param ... [any]\cr
#'   Further parameters passed to internally used \code{\link{makeRegistry}}.
#' @return [\code{Registry}]. Auto-created registry for the job.
#' @export
submitFunCall = function(id, fun, pars, file, seed, resources=list(),
  wait=function(retries) 50L * 2L^retries, max.retries=10L, ...) {

  checkArg(id, "character", len=1L, na.ok=FALSE)
  checkArg(fun, "function")
  checkArg(pars, "list")
  checkArg(file, "character", len=1L, na.ok=FALSE)
  if (missing(seed)) {
    seed = getRandomSeed()
  } else {
    seed = convertInteger(seed)
    checkArg(seed, cl = "integer", len = 1L, na.ok = FALSE)
  }
  # get a unique, unused tempdir. tempdir() always stays the same per session
  td = tempfile(pattern="")
  reg = makeRegistry(id=id, file.dir=td, sharding=FALSE, ...)
  fun2 = function(fun, pars, file) {
    result = do.call(fun, pars)
    save(file=file, result)
  }
  job = makeJob(id=1L, fun=fun2, pars=list(fun=fun, pars=pars, file=file), seed=seed)
  addJob(reg, job)
  submitJobs(reg, resources=resources)
  return(reg)
}
