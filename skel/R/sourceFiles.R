#' Set and source files relative to your working directory.
#'
#' The registry holds a list of files, relative to your working directory, which will be sourced 
#' on both master on slave when the \code{\link{Registry}} is loaded.
#' This function allows managing the list.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param files [\code{character}]\cr
#'   Overwrites the list of files with \code{files}.
#'   May be missing.
#'   Files must be specified relative to the registry's \code{work.dir}.
#' @param source.now [\code{logical}]\cr
#'   If \code{TRUE} (default), all files will immediately be sourced.
#' @param envir [\code{environment}]\cr
#'   Environment to source the files into. Default is the global environment.
#' @return Updated registry.
#' @export
sourceFiles = function(reg, files, source.now = TRUE, envir = .GlobalEnv) {
  checkRegistry(reg)
  checkArg(source.now, "logical", len=1L, na.ok=FALSE)

  if (!missing(files)) {
    checkArg(files, "character", na.ok=FALSE)
    ok = file.exists(file.path(reg$work.dir, files))
    if (!all(ok))
      stopf("Files not found: %s", collapse(files[!ok]))
    reg$src.files = files
    saveRegistry(reg)
  }

  if (source.now) {
    if (length(reg$src.files)) {
      checkArg(envir, "environment")
      messagef("Sourcing %i files: %s", length(reg$src.files), collapse(basename(reg$src.files)))
      lapply(file.path(reg$work.dir, reg$src.files), sys.source, envir = envir)
    }
  }

  invisible(reg)
}
