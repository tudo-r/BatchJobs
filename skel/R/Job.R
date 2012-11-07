#' Creates a job description.
#'
#' Usually you will not do this manually.
#' Every object is a list that contains the passed arguments of the constructor.
#'
#' @param id [\code{integer(1)}]\cr
#'   Job id, determined by DB autoincrement.
#'   Default is \code{NA}.
#' @param fun [\code{function}]\cr
#'   Job function to apply on parameters.
#' @param fun.id [\code{character(1)}]\cr
#'   Id used to store function on disk.
#'   Default is \code{digest(fun)}.
#' @param pars [\code{list}]\cr
#'   Parameter list for job function.
#' @param seed [\code{integer(1)}]\cr
#'   Random seed for job.
#' @aliases Job
#' @export
makeJob = function(id=NA_integer_, fun, fun.id=digest(fun), pars, seed) {
  setClasses(list(id=id, fun=fun, fun.id=fun.id, pars=pars, seed=seed), "Job")
}

#' Get number of jobs in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer(1)}].
#' @export
getJobNr = function(reg) {
  checkArg(reg, "Registry")
  dbGetJobCount(reg)
}

#' Get ids of jobs in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getJobIds = function(reg) {
  checkArg(reg, "Registry")
  dbGetJobIds(reg)
}

#' @S3method print Job
print.Job = function(x, ...) {
  cat("BatchJobs job:\n")
  catf("  Job id: %s", x$id)
  catf("  Fun id: %s", x$fun.id)
  catf("  Fun formals: %s", collapse(names(formals(x$fun))))
  catf("  Pars: %s", listToShortString(x$pars))
  catf("  Seed: %i", x$seed)
}
