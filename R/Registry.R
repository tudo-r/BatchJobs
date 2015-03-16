makeRegistryInternal = function(id, file.dir, sharding, work.dir,
  multiple.result.files, seed, packages, src.dirs, src.files) {

  checkIdValid(id, allow.minus = FALSE)

  assertString(file.dir)
  checkDir(file.dir, create = TRUE, check.empty = TRUE, check.posix = TRUE, msg = TRUE)
  file.dir = sanitizePath(file.dir, make.absolute = TRUE)

  if (missing(work.dir))
    work.dir = getwd()
  else
    assertString(work.dir)
  checkDir(work.dir, check.posix = TRUE)
  work.dir = sanitizePath(work.dir, make.absolute = TRUE)

  assertFlag(sharding)
  assertFlag(multiple.result.files)
  seed = if (missing(seed)) getRandomSeed() else asInt(seed)

  assertCharacter(packages, any.missing = FALSE)
  packages = union(packages, "BatchJobs")
  requirePackages(packages, stop = TRUE, suppress.warnings = TRUE, default.method = "attach")

  assertCharacter(src.dirs, any.missing = FALSE)
  src.dirs = sanitizePath(src.dirs, make.absolute = FALSE)
  assertCharacter(src.files, any.missing = FALSE)
  src.files = sanitizePath(src.files, make.absolute = FALSE)

  # make paths absolute to be sure. otherwise cfSSH wont work for example
  # also check the dirs
  # file dir
  # job dir
  job.dir = getJobParentDir(file.dir)
  checkDir(job.dir, create = TRUE, check.empty = TRUE)
  # fun dir
  fun.dir = getFunDir(file.dir)
  checkDir(fun.dir, create = TRUE, check.empty = TRUE)
  # resources, pending, exports, work.dir
  checkDir(getResourcesDir(file.dir), create = TRUE, check.empty = TRUE)
  checkDir(getPendingDir(file.dir), create = TRUE, check.empty = TRUE)
  checkDir(getExportDir(file.dir), create = TRUE, check.empty = TRUE)
  sourceRegistryFilesInternal(work.dir, src.dirs, src.files)

  packages = setNames(lapply(packages, function(pkg) list(version = packageVersion(pkg))), packages)
  conf = getConfig()

  setClasses(list(
    id = id,
    version = R.version,
    RNGkind = RNGkind(),
    db.driver = conf$db.driver,
    db.options = conf$db.options,
    seed = seed,
    file.dir = file.dir,
    sharding = sharding,
    work.dir = work.dir,
    src.dirs = src.dirs,
    src.files = src.files,
    multiple.result.files = multiple.result.files,
    packages = packages[order(names(packages))]
  ), "Registry")
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
#' @param src.dirs [\code{character}]\cr
#'   Directories containing R scripts
#'   to be sourced on registry load (both on slave and master).
#'   Files not matching the pattern \dQuote{\\.[Rr]$} are ignored.
#'   Useful if you have many helper functions that are needed during the execution of your jobs.
#'   These files should only contain function definitions and no executable code.
#'   Default is \code{character(0)}.
#' @param src.files [\code{character}]\cr
#'   R scripts files
#'   to be sourced on registry load (both on slave and master).
#'   Useful if you have many helper functions that are needed during the execution of your jobs.
#'   These files should only contain function and constant definitions and no long running, executable code.
#'   These paths are considered to be relative to your \code{work.dir}.
#'   As a last remedy in problematic cases you can use absolute paths, by passing paths that
#'   start with \dQuote{/}, see the comment about \code{file.dir} and \code{work.dir} above,
#'   where we allow the same thing.
#'   Note that this is a less portable approach and therefore usually a less good idea.
#'   Default is \code{character(0)}.
#' @param skip [\code{logical(1)}]\cr
#'   Skip creation of a new registry if a registry is found in \code{file.dir}.
#'   Defaults to \code{TRUE}.
#' @return [\code{\link{Registry}}]
#' @aliases Registry
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' print(reg)
makeRegistry = function(id, file.dir, sharding = TRUE, work.dir, multiple.result.files = FALSE,
                        seed, packages = character(0L), src.dirs = character(0L), src.files = character(0L),
                        skip = TRUE) {
  if (missing(file.dir))
    file.dir = file.path(getwd(), paste0(id, "-files"))
  assertFlag(skip)
  if (skip && isRegistryDir(file.dir))
    return(loadRegistry(file.dir = file.dir))

  reg = makeRegistryInternal(id, file.dir, sharding, work.dir, multiple.result.files, seed, packages,
    src.dirs, src.files)

  dbCreateJobStatusTable(reg)
  dbCreateJobDefTable(reg)
  saveRegistry(reg)
  reg
}

#' @export
print.Registry = function(x, ...) {
  cat("Job registry: ",  x$id, "\n")
  cat("  Number of jobs: ", dbGetJobCount(x), "\n")
  cat("  Files dir:", x$file.dir, "\n")
  cat("  Work dir:", x$work.dir, "\n")
  cat("  Multiple result files:", x$multiple.result.files, "\n")
  cat("  Seed:", x$seed, "\n")
  cat("  Required packages:", collapse(names(x$packages), ", "), "\n")
}

#' @title Load a previously saved registry.
#'
#' @details
#' Loads a previously created registry from the file system.
#' The \code{file.dir} is automatically updated upon load, so be careful
#' if you use the registry on multiple machines simultaneously, e.g.
#' via sshfs or a samba share.
#'
#' @param file.dir [\code{character(1)}]\cr
#'   Location of the file.dir to load the registry from.
#' @param work.dir [\code{character(1)}]\cr
#'   Location of the work. Unchanged if missing.
#' @return [\code{\link{Registry}}].
#' @export
loadRegistry = function(file.dir, work.dir) {
  fn = getRegistryFilePath(file.dir)
  if (!file.exists(fn))
    stopf("No registry found in '%s'", file.dir)
  info("Loading registry: %s", fn)
  reg = load2(fn, "reg")

  requirePackages(names(reg$packages), why = sprintf("registry %s", reg$id), default.method = "attach")

  if (!isOnSlave()) {
    # FIXME: check that no jobs are running, if possible, before updating
    adjusted = adjustRegistryPaths(reg, file.dir, work.dir)
    if (!isFALSE(adjusted))
      reg = adjusted

    updated = updateRegistry(reg)
    if (!isFALSE(updated))
      reg = updated

    if (!isFALSE(adjusted) || !isFALSE(updated))
      saveRegistry(reg)
  }
  loadExports(reg)
  sourceRegistryFiles(reg)

  return(reg)
}

saveRegistry = function(reg) {
  fn = getRegistryFilePath(reg$file.dir)
  info("Saving registry: %s", fn)
  save(file = fn, reg)
  reg
}

isRegistryDir = function(dir) {
  isDirectory(dir) && file.exists(getRegistryFilePath(dir))
}

checkRegistry = function(reg, strict = FALSE) {
  cl = class(reg)
  expected = "Registry"
  if (strict) {
    if (head(cl, 1L) != expected)
      stopf("Registry class mismatch: Expected argument with first class '%s'", expected)
  } else {
    if (expected %nin% cl)
      stopf("Registry class mismatch: Expected argument of class '%s'", expected)
  }
  invisible(TRUE)
}


#' Remove a registry object.
#'
#' If there are no live/running jobs, the registry will be closed
#' and all of its files will be removed from the file system.
#' If there are live/running jobs, an informative error is generated.
#' The default is to prompt the user for confirmation.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ask [\code{character(1)}]\cr
#'   If \code{"yes"} the user is prompted to confirm the action.
#'   If trying to prompt the user this way in a non-interactive
#'   session, then an informative error is generated.
#'   If \code{"no"}, the registry will be removed without
#'   further confirmation.
#'
#' @return [\code{logical[1]}]
#'
#' @export
removeRegistry = function(reg, ask = c("yes", "no")) {
  ask = match.arg(ask)

  if (ask == "yes") {
    if (!interactive())
      stopf("removeRegistry(..., ask = \"yes\") only works in interactive sessions.")
    prompt = sprintf("Are you sure you wish to delete BatchJobs registry '%s' and all of it's files in directory '%s'? [y/N]: ", reg$id, reg$file.dir)
    ans = 2L
    repeat {
      ans = tolower(readline(prompt))
      ans = gsub("[ ]", "", ans)
      if (ans == "") ans = "no"
      ans = pmatch(ans, table=c("yes", "no"), nomatch=0L)
      if (ans > 0L) break
    }
    if (ans != 1L) return(invisible(FALSE))
  }


  checkRegistry(reg)
  syncRegistry(reg)

  running = findOnSystem(reg)
  if (length(running) > 0L)
    stopf("Can't remove registry, because there are %d live jobs on the system.", length(running))

  ## FIXME: Close database first?

  removeDirs(reg$file.dir, recursive=TRUE, must.work=TRUE)
}
