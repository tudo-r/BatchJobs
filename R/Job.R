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
#' @param name [\code{character(1)}]\cr
#'   Alias name for this job.
#' @param seed [\code{integer(1)}]\cr
#'   Random seed for job.
#' @aliases Job
#' @export
makeJob = function(id = NA_integer_, fun, fun.id = digest(fun), pars, name, seed) {
  setClasses(list(id = id, fun = fun, fun.id = fun.id, pars = pars, name = name, seed = seed), "Job")
}

#' Get number of jobs in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{integer(1)}].
#' @export
getJobNr = function(reg) {
  checkRegistry(reg)
  dbGetJobCount(reg)
}

#' Get ids of jobs in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getJobIds = function(reg) {
  checkRegistry(reg)
  dbGetJobIds(reg)
}

#' @export
print.Job = function(x, ...) {
  cat("BatchJobs job:\n")
  catf("  Job id: %s", x$id)
  catf("  Fun id: %s", x$fun.id)
  catf("  Fun formals: %s", collapse(names(formals(x$fun))))
  catf("  Name: %s", x$name)
  catf("  Seed: %i", x$seed)
  catf("  Pars: %s", convertToShortString(x$pars))
}
