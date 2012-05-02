makeRegistryInternal = function(id, file.dir, sharding,
  work.dir, multiple.result.files, seed, packages) {
  cur.dir = getwd()
  checkArg(id, cl = "character", len = 1L, na.ok = FALSE)
  checkIdValid(id)
  if (missing(file.dir))
    file.dir = file.path(cur.dir, paste(id, "files", sep="-"))
  checkArg(file.dir, cl = "character", len = 1L, na.ok = FALSE)
  checkArg(sharding, cl = "logical", len = 1L, na.ok = FALSE)
  if (missing(work.dir))
    work.dir = cur.dir
  checkArg(work.dir, cl = "character", len = 1L, na.ok = FALSE)
  checkArg(multiple.result.files, cl = "logical", len = 1L, na.ok = FALSE)

  if (missing(seed)) {
    seed = getRandomSeed()
  } else {
    seed = convertInteger(seed)
    checkArg(seed, cl = "integer", len = 1L, lower = 1L, na.ok = FALSE)
  }
  checkArg(packages, cl = "character", na.ok = FALSE)
  requirePackages(packages, stop=TRUE, suppress.warnings=TRUE)

  # make paths absolute to be sure. otherwise cfSSH wont work for example
  checkDir(file.dir, create=TRUE, check.empty=TRUE, check.posix=TRUE, msg=TRUE)
  file.dir = makePathAbsolute(file.dir)
  job.dir = getJobParentDir(file.dir)
  checkDir(job.dir, create=TRUE, check.empty=TRUE)
  fun.dir = getFunDir(file.dir)
  checkDir(fun.dir, create=TRUE, check.empty=TRUE)
  checkDir(work.dir, check.posix=TRUE)
  work.dir = makePathAbsolute(work.dir)

  packages = structure(lapply(packages, function(pkg) list(version = packageVersion(pkg))),
                       names = packages)
  structure(list(
    id = id,
    version = R.version,
    RNGkind = RNGkind(),
    db.driver = "SQLite",
    db.options = list(),
    seed = seed,
    file.dir = file.dir,
    sharding = sharding,
    work.dir = work.dir,
    multiple.result.files = multiple.result.files,
    packages = packages[order(names(packages))]
  ), class = "Registry")
}


#' Construct a registry object.
#'
#' Note that if you don't want links in your paths (\code{file.dir}, \code{work.dir}) to get resolved and have
#' complete control over the way the path is used internally, pass an absolute path which begins with \dQuote{/}.
#'
#' Every object is a list that contains the passed arguments of the constructor.
#
#' @param id [\code{character(1)}]\cr
#'   Name of registry. Displayed e.g. in mails or in cluster queue.
#' @param file.dir [\code{character(1)}]\cr
#'   Path where files regarding the registry / jobs should be saved.
#'   Default is \dQuote{<id>-files} in current working directory if \code{id} is set.
#' @param sharding [\code{logical(1)}]\cr
#'   Enable sharding to distribute result files into different subdirectories?
#'   Important if you have many experiments.
#'   Default is \code{TRUE}.
#' @param work.dir [\code{character(1)}]\cr
#'   Working directory for R process when experiment is executed.
#'   Default is the current working directory when registry is created.
#' @param multiple.result.files [\code{logical(1)}]\cr
#'   Should a result file be generated for every list element of the
#'   returned list of the job function?
#'   Note that the function provided to \code{\link{batchMap}} or
#'   \code{\link{batchReduce}} must return a named list if this is set to \code{TRUE}.
#'   The result file will be named \dQuote{<id>-result-<element name>.RData}
#'   instead of \dQuote{<id>-result.RData}.
#'   Default is \code{FALSE}.
#' @param seed [\code{integer(1)}]\cr
#'   Start seed for experiments. The first experiment in the registry will use this
#'   seed, for the subsequent ones the seed is incremented by 1.
#'   Default is a random number from 1 to \code{.Machine$integer.max/2}.
#' @param packages [\code{character}]\cr
#'   Packages that will always be loaded on each node.
#'   Default is \code{character(0)}.
#' @return [\code{\link{Registry}}]
#' @aliases Registry
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' print(reg)
makeRegistry = function(id, file.dir, sharding=TRUE,
  work.dir, multiple.result.files = FALSE, seed, packages=character(0L)) {
  reg = makeRegistryInternal(id, file.dir, sharding, work.dir,
                             multiple.result.files, seed, union(packages, "BatchJobs"))
  dbCreateJobStatusTable(reg)
  dbCreateJobDefTable(reg)
  saveRegistry(reg)
  reg
}

#' @S3method print Registry
print.Registry = function(x, ...) {
  cat("Job registry: ",  x$id, "\n")
  cat("  Number of jobs: ", dbGetJobCount(x), "\n")
  cat("  Files dir:", x$file.dir, "\n")
  cat("  Work dir:", x$work.dir, "\n")
  cat("  Multiple result files:", x$multiple.result.files, "\n")
  cat("  Seed:", x$seed, "\n")
  cat("  Required packages:", paste(names(x$packages), collapse=", "), "\n")
}

#' Load a previously saved registry.
#' @param file.dir [\code{character(1)}]\cr
#'   Location of the file.dir to load the registry from.
#' @param save [\code{logical(1)}]\cr
#'   Set \code{file.dir} in the registry and save.
#'   Useful if you moved the file dir, because you wanted to continue
#'   working somewhere else.
#'   Default is \code{FALSE}.
#' @return [\code{\link{Registry}}].
#' @export
loadRegistry = function(file.dir, save=FALSE) {
  fn = getRegistryFilePath(file.dir)
  message("Loading registry: ", fn)
  reg = load2(fn, "reg")
  if(save) {
    reg$file.dir = makePathAbsolute(file.dir)
    saveRegistry(reg)
  }
  reg
}

saveRegistry = function(reg) {
  fn = getRegistryFilePath(reg$file.dir)
  message("Saving registry: ", fn)
  save(file=fn, reg)
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

