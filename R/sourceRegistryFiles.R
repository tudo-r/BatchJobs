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
sourceRegistryFiles = function(reg, envir=.GlobalEnv) {
  checkRegistry(reg)
  checkArg(envir, "environment")
  sourceRegistryFilesInternal(reg$work.dir, reg$src.dirs, reg$src.files)
}

sourceRegistryFilesInternal = function(work.dir, dirs, files, envir=.GlobalEnv) {
  f1 = getRScripts(file.path(work.dir, dirs))
  f2 = file.path(work.dir, files)
  ok = file.exists(f2)
  if (any(!ok))
    stopf("Files to source not found, e.g. %s", head(f2[!ok], 1L))
  lapply(c(f1, f2), sys.source, envir=envir)
  invisible(NULL)
}

getRScripts = function(dirs) {
  if (length(dirs)) {
    ok = isDirectory(dirs)
    if (any(!ok))
      stopf("Directories not found: %s", collapse(dirs[!ok]))
    unlist(lapply(dirs, list.files, pattern = "\\.[Rr]$", full.names=TRUE))
  } else {
    character(0L)
  }
}
