#' Load exported R data objects stored.
#'
#' Loads all \code{RData} files in the \dQuote{exports} subdirectory of your \code{file.dir}
#' and assigns the objects to the global environment.
#'
#' @param reg [\code{Registry}]\cr
#'   Location of the file.dir to load the registry from.
#' @return Invisibly returns a character vector of loaded file names.
#' @export
loadExports = function(reg) {
  checkRegistry(reg)
  f = fail(getExportDir(reg$file.dir), extension="RData", simplify=FALSE)
  keys = f$ls()
  if (length(keys)) {
    messagef("Loading %i RData files: %s", length(keys), collapse(keys))
    f$assign(keys, envir=.GlobalEnv)
  }
  invisible(sprintf("%s.RData", keys))
}
