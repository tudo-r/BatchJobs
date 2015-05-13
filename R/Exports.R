#' @title Load exported R data objects.
#'
#' @description
#' Loads exported \code{RData} object files in the \dQuote{exports} subdirectory of your \code{file.dir}
#' and assigns the objects to the global environment.
#'
#' @template arg_reg
#' @param what [\code{character}]\cr
#'   Names of objects and corresponding \dQuote{RData} files which should be loaded.
#'   Default \code{NULL} loads all files.
#' @return [\code{character}]. Invisibly returns a character vector of loaded objects.
#' @family exports
#' @export
loadExports = function(reg, what = NULL) {
  checkRegistry(reg)
  if (!is.null(what))
    assertCharacter(what, any.missing = FALSE)
  f = fail(getExportDir(reg$file.dir), extension = "RData", simplify = FALSE)
  keys = f$ls()
  if (!is.null(what))
    keys = intersect(keys, what)
  if (length(keys) > 0L) {
    messagef("Loading RData files: %s", collapse(keys))
    f$assign(keys, envir = .GlobalEnv)
  }
  invisible(keys)
}

#' @title Export R object to be available on the slaves.
#'
#' @description
#' Saves objects as \code{RData} files in the \dQuote{exports} subdirectory of your \code{file.dir}
#' to be later loaded on the slaves.
#'
#' @template arg_reg
#' @param ... [any]\cr
#'   Objects to export. You must provide a valid name.
#' @param li [\code{list}]\cr
#'   More objects to export provided as a named list.
#' @param overwrite [\code{logical(1)}]\cr
#'   If set to \code{FALSE} (default), exported objects are protected from being overwritten
#'   by multiple calls of this function. Setting this to \code{TRUE} disables this check.
#' @return [\code{character}]. Invisibly returns a character vector of exported objects.
#' @family exports
#' @export
batchExport = function(reg, ..., li = list(), overwrite = FALSE) {
  checkRegistry(reg)
  ddd = list(...)
  assertList(li, names = "strict")
  assertList(ddd, names = "strict")
  assertFlag(overwrite)
  keys = c(names(li), names(ddd))
  dup = anyDuplicated(keys)
  if (dup > 0L)
    stopf("Object for export provided more than once: '%s'", keys[dup])

  f = fail(getExportDir(reg$file.dir), extension = "RData", simplify = FALSE)

  if (!overwrite) {
    collision = which.first(keys %in% f$ls())
    if (length(collision) > 0L)
      stopf("Object named '%s' already exported and 'overwrite' is set to FALSE", keys[collision])
  }

  f$put(li = li)
  f$put(li = ddd)
  invisible(keys)
}

#' @title Unload exported R objects.
#'
#' @description
#' Removes \code{RData} files from the \dQuote{exports} subdirectory of your \code{file.dir}
#' and thereby prevents loading on the slave.
#'
#' @template arg_reg
#' @param what [\code{character}]\cr
#'   Names of objects to remove.
#' @return [\code{character}]. Invisibly returns a character vector of unexported objects.
#' @family exports
#' @export
batchUnexport = function(reg, what) {
  checkRegistry(reg)
  assertCharacter(what, any.missing = FALSE)

  f = fail(getExportDir(reg$file.dir), extension = "RData", simplify = FALSE)
  keys = intersect(f$ls(), what)
  f$remove(keys)
  invisible(keys)
}
