#' Source registry files
#'
#' Sources all files found in \code{src.dirs} and specified via \code{src.files}.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param envir [\code{environment}]\cr
#'   Environment to source the files into. Default is the global environment.
#' @return Nothing.
#' @export
sourceRegistryFiles = function(reg, envir = .GlobalEnv) {
  checkRegistry(reg)
  assertEnvironment(envir)
  sourceRegistryFilesInternal(reg$work.dir, reg$src.dirs, reg$src.files)
}

sourceRegistryFilesInternal = function(work.dir, dirs, files, envir = .GlobalEnv) {
  # add work dir if not /foo/bar path
  addWorkDir = function(path) {
    ifelse(isPathFromRoot(path), path, file.path(work.dir, path))
  }
  dirs = vcapply(dirs, addWorkDir)
  files = vcapply(files, addWorkDir)
  dirs.files = getRScripts(dirs)
  ok = file.exists(files)
  if (any(!ok))
    stopf("Files to source not found, e.g. %s", head(files[!ok], 1L))
  lapply(c(dirs.files, files), sys.source, envir = envir)
  invisible(NULL)
}

getRScripts = function(dirs) {
  if (length(dirs)) {
    ok = isDirectory(dirs)
    if (any(!ok))
      stopf("Directories not found: %s", collapse(dirs[!ok]))
    unlist(lapply(dirs, list.files, pattern = "\\.[Rr]$", full.names = TRUE))
  } else {
    character(0L)
  }
}
