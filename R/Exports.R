#' @title Load exported R data objects.
#'
#' @description
#' Loads exported \code{RData} object files in the \dQuote{exports} subdirectory of your \code{file.dir}
#' and assigns the objects to the global environment.
#'
#' @template arg_reg
#' @param what [\code{character}]\cr
#'   Names of objects to load. Defaults to all objects exported.
#' @return [\code{character}]. Invisibly returns a character vector of loaded objects.
#' @family exports
#' @export
loadExports = function(reg, what = NULL) {
  checkRegistry(reg, writeable = FALSE)
  if (!is.null(what))
    assertCharacter(what, any.missing = FALSE)
  path = getExportDir(reg$file.dir)
  fns = list.files(path, pattern = "\\.RData$", all.files = TRUE)
  keys = gsub("\\.RData$", "", fns)
  if (!is.null(what)) {
    i = which(keys %in% what)
    fns = fns[i]
    keys = keys[i]
  }

  if (length(fns) > 0L) {
    messagef("Loading RData files: %s", collapse(keys))
    for (i in seq_along(fns))
      assign(keys[i], load2(file.path(path, fns[i]), envir = .GlobalEnv))
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
  checkRegistry(reg, writeable = FALSE)
  ddd = list(...)
  assertList(li, names = "strict")
  assertList(ddd, names = "strict")
  assertFlag(overwrite)

  keys = c(names(li), names(ddd))
  dup = anyDuplicated(keys)
  if (dup > 0L)
    stopf("Object for export provided more than once: '%s'", keys[dup])
  path = getExportDir(reg$file.dir)

  if (!overwrite) {
    fns = list.files(path, pattern = "\\.RData$", all.files = TRUE)
    old.keys = gsub("\\.RData$", "", fns)
    collision = which.first(keys %in% old.keys)
    if (length(collision) > 0L)
      stopf("Object named '%s' already exported and 'overwrite' is set to FALSE", keys[collision])
  }

  objs = list2env(c(li, ddd))
  for (i in seq_along(keys))
    save(list = keys[i], envir = objs, file = file.path(path, sprintf("%s.RData", keys[i])))
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
  checkRegistry(reg, writeable = FALSE)
  assertCharacter(what, any.missing = FALSE)

  path = getExportDir(reg$file.dir)
  fns = list.files(path, pattern = "\\.RData$", all.files = TRUE)
  keys = gsub("\\.RData$", "", fns)

  i = which(keys %in% what)
  file.remove(file.path(path, fns[i]))
  invisible(keys[i])
}
