#' Source files in source directories specified in the registry.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param envir [\code{environment}]\cr
#'   Environment to source the files into. Default is the global environment.
#' @return Nothing.
#' @export
sourceRegistryFiles = function(reg, envir = .GlobalEnv) {
  checkRegistry(reg)
  sourceRegistryFilesInternal(reg$work.dir, reg$src.dirs, envir)
}

sourceRegistryFilesInternal = function(work.dir, src.dirs, envir = .GlobalEnv) {
  if (length(src.dirs)) {
    checkArg(envir, "environment")
    dirs = file.path(work.dir, src.dirs)
    ok = isDirectory(dirs)
    if (any(!ok))
      stopf("Directories not found: %s", collapse(dirs[!ok]))
    for (dir in dirs) {
      f = list.files(dir, pattern = "\\.[Rr]$", full.names=TRUE)
      messagef("Sourcing %i files in %s...", length(f), dir)
      lapply(f, sys.source, envir = envir)
    }
  }

  invisible(NULL)
}
