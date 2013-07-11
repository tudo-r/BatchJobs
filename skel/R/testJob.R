#' Tests a job by running it with Rscript in a new process.
#'
#' Useful for debugging.
#' Note that neither the registry, database or file directory are changed.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job to test.
#'   Default is first job id of registry.
#' @param resources [\code{list}]\cr
#'   Usually not needed, unless you call the function \code{\link{getResources}} in your job.
#'   See  \code{\link{submitJobs}}.
#'   Default is empty list.
#' @return [any]. Result of job. If the job did not complete because of an error, NULL is returned.
#' @seealso \code{\link{reduceResults}}
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) if (x==1) stop("oops") else x
#' batchMap(reg, f, 1:2)
#' testJob(reg, 1)
#' testJob(reg, 2)
testJob = function(reg, id, resources=list()) {
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
  checkArg(resources, "list")
  resources = resrc(resources)

  # we dont want to change anything in the true registry / file dir / DB
  # so we have to copy stuff a little bit
  r = reg

  # get a unique, unused tempdir. tmpdir() always stays the same per session
  td = tempfile(pattern="")
  construct = sprintf("make%s", class(r)[1L])

  # copy reg
  reg = do.call(construct, list(id=reg$id, seed=r$seed, file.dir=td, work.dir=r$work.dir,
    sharding=FALSE, multiple.result.files=r$multiple.result.files,
    packages=names(reg$packages)))

  # copy DB
  file.copy(from=file.path(r$file.dir, "BatchJobs.db"), to=file.path(td, "BatchJobs.db"), overwrite=TRUE)

  # copy conf
  conf = getBatchJobsConf()
  save(file = getConfFilePath(reg), conf)

  # copy job stuff
  copyRequiredJobFiles(r, reg, id)

  # write r script
  resources.timestamp = saveResources(reg, resources)
  writeRscripts(reg, getClusterFunctions(conf), id, chunks.as.arrayjobs=FALSE, resources.timestamp=resources.timestamp,
                disable.mail=TRUE, delays=0, interactive.test=FALSE)

  # execute
  rhome = Sys.getenv("R_HOME")
  cmd = sprintf("%s/bin/Rscript %s", rhome, getRScriptFilePath(reg, id))
  stime = Sys.time()
  message("### Output of new R process starts here ###")
  system(cmd, wait=TRUE)
  message("### Output of new R process ends here ###")
  messagef("### Approximate running time: %.2f seconds", as.numeric(Sys.time() - stime))
  res = try(getResult(reg, id))
  if (is.error(res))
    return(NULL)
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
#' @export
copyRequiredJobFiles = function(reg1, reg2, id) {
  UseMethod("copyRequiredJobFiles")
}

#' @S3method copyRequiredJobFiles Registry
copyRequiredJobFiles.Registry = function(reg1, reg2, id) {
  job = getJob(reg1, id, load.fun=TRUE, check.id=FALSE)
  file.copy(getFunFilePath(reg1, job$fun.id), getFunFilePath(reg2, job$fun.id))
}


