#' @title Load a previously saved registry.
#'
#' @description
#' Loads a previously created registry from the file system.
#' The \code{file.dir} is automatically updated upon load if \code{adjust.paths} is set to
#' \code{TRUE}, so be careful if you use the registry on multiple machines simultaneously,
#' e.g. via sshfs or a samba share.
#'
#' There is a heuristic included which tries to detect if the location of the registry
#' has changed and returns a read-only registry if necessary.
#'
#' @param file.dir [\code{character(1)}]\cr
#'   Location of the file.dir to load the registry from.
#' @param work.dir [\code{character(1)}]\cr
#'   Location of the work. Unchanged if missing.
#' @param adjust.paths [\code{logical(1)}]\cr
#'   If set to \code{FALSE} (default), the paths for the \code{file.dir} and \code{work.dir}
#'   will not be updated. Set to  \code{TRUE} if you moved the directoy to another system
#'   \emph{after} all computations finished.
#' @return [\code{\link{Registry}}].
#' @export
loadRegistry = function(file.dir, work.dir, adjust.paths = FALSE) {
  assertString(file.dir)
  assertFlag(adjust.paths)
  fn = getRegistryFilePath(file.dir)
  if (!file.exists(fn))
    stopf("No registry found in '%s'", file.dir)
  info("Loading registry: %s", fn)
  reg = load2(fn, "reg")

  requirePackages(names(reg$packages), why = sprintf("registry %s", reg$id), default.method = "attach")

  if (!isOnSlave()) {
    save.reg = FALSE
    read.only = FALSE

    adjusted = adjustRegistryPaths(reg, file.dir, work.dir)
    if (!isFALSE(adjusted)) {
      if (!adjust.paths) {
        warning("It seems like you've moved the registry to a new location or system. ",
                 "To update the paths, call 'loadRegistry' with option 'adjust.paths' set to TRUE. ",
                 "But make sure that there are no jobs running on the system. ",
                 "Returning a read-only registry, and not updating the database to the latest layout, ",
                 "i.e. your registry may be defunct.")
        read.only = TRUE
      }
      reg = adjusted
      save.reg = TRUE
    }

    if (!read.only) {
      updated = updateRegistry(reg)
      if (!isFALSE(updated)) {
        reg = updated
        save.reg = TRUE
      }
    }

    if (save.reg) {
      if (read.only)
        reg$read.only = TRUE
      else
        saveRegistry(reg)
    }
  }

  loadExports(reg)
  sourceRegistryFiles(reg)
  return(reg)
}
