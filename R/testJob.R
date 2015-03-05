#' Tests a job by running it with Rscript in a new process.
#'
#' @description
#' Useful for debugging.
#' Note that neither the registry, database or file directory are changed.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job to test.
#'   Default is first job id of registry.
#' @param external [\code{logical(1)}]\cr
#'   Run test in an independent external R session instead of current.
#'   The former allows to uncover missing variable definitions (which may
#'   accidentially be defined in the current global environment) and the latter
#'   is useful to get traceable execeptions.
#'   Default is \code{TRUE}.
#' @param resources [\code{list}]\cr
#'   Usually not needed, unless you call the function \code{\link{getResources}} in your job.
#'   See  \code{\link{submitJobs}}.
#'   Default is empty list.
#' @return [any]. Result of job. If the job did not complete because of an error, NULL is returned.
#' @family debug
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) if (x==1) stop("oops") else x
#' batchMap(reg, f, 1:2)
#' testJob(reg, 2)
testJob = function(reg, id, resources = list(), external = TRUE) {
  checkRegistry(reg)
  #syncRegistry(reg)
  if (missing(id)) {
    id = dbGetJobId(reg)
    if (length(id) == 0L)
      stop("Registry is empty!")
    messagef("Testing job with id=%i ...", id)
  } else {
    id = checkId(reg, id)
  }
  assertList(resources)
  resources = resrc(resources)
  assertFlag(external)

  if (external) {
    # we dont want to change anything in the true registry / file dir / DB
    # so we have to copy stuff a little bit
    r = reg

    # get a unique, unused tempdir. tmpdir() always stays the same per session
    td = tempfile(pattern = "")
    construct = sprintf("make%s", class(r)[1L])

    # copy reg
    reg = do.call(construct, list(id = reg$id, seed = r$seed, file.dir = td, work.dir = r$work.dir,
      sharding = FALSE, multiple.result.files = r$multiple.result.files,
      packages = names(reg$packages), src.dirs = reg$src.dirs, src.files = reg$src.files))

    # copy DB
    file.copy(from = file.path(r$file.dir, "BatchJobs.db"), to = file.path(td, "BatchJobs.db"), overwrite = TRUE)

    # copy conf
    saveConf(reg)

    # copy job stuff
    copyRequiredJobFiles(r, reg, id)

    # copy exports
    file.copy(from = getExportDir(r$file.dir), to = td, recursive = TRUE)

    # write r script
    resources.timestamp = saveResources(reg, resources)
    writeRscripts(reg, getClusterFunctions(getBatchJobsConf()), id, chunks.as.arrayjobs = FALSE,
      resources.timestamp = resources.timestamp, disable.mail = TRUE, delays = 0)

    # execute
    now = Sys.time()
    message("### Output of new R process starts here ###")
    system3(file.path(R.home("bin"), "Rscript"), getRScriptFilePath(reg, id), wait = TRUE)
    message("### Output of new R process ends here ###")
    dt = difftime(Sys.time(), now)
    messagef("### Approximate running time: %.2f %s", as.double(dt), units(dt))

    res = try(getResult(reg, id))
    if (is.error(res))
      return(NULL)
  } else {
    setOnSlave(TRUE)
    on.exit(setOnSlave(FALSE))
    # FIXME: stuff we might need to store before: resources
    saveConf(reg)

    # trigger loadExports, sourceRegistryFiles, ...
    loadRegistry(reg$file.dir)
    res = applyJobFunction(reg, getJob(reg, id), makeFileCache(FALSE))
  }
  return(res)
}

#' ONLY FOR INTERNAL USAGE.
#' @param reg1 [\code{\link{Registry}}]\cr
#'   Source registry.
#' @param reg2 [\code{\link{Registry}}]\cr
#'   Detination registry.
#' @param id [\code{character(1)}]\cr
#'   Job id.
#' @return Nothing.
#' @keywords internal
#' @export
copyRequiredJobFiles = function(reg1, reg2, id) {
  UseMethod("copyRequiredJobFiles")
}

#' @export
copyRequiredJobFiles.Registry = function(reg1, reg2, id) {
  job = getJob(reg1, id, check.id = FALSE)
  file.copy(getFunFilePath(reg1, job$fun.id), getFunFilePath(reg2, job$fun.id))
}
