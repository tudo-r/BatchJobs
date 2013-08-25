#' Export objects to be loaded into the global environment.
#'
#' Export objects to be automatically loaded on slave and master
#' when the registry gets loaded.
#' The argument \dQuote{trigger} controls when the objects will be
#' assigned in the global environment: Only on the slaves (\dQuote{slave}),
#' only on the master (\dQuote{master}) or on both slaves and master
#' (\dQuote{master}).
#' Note that this imediately assigns a copy of passed objects
#' in the current R session.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ... [any]\cr
#'   Objects to export. Must be provided in a key = value syntax.
#'   If names are missing, a heuristic will try to look them up.
#' @param li [\code{list]}\cr
#'   Named list with additional objects to export.
#'   Default is an empty list (\code{list()}).
#' @param trigger [\code{character(1)}]\cr
#'   Three triggers for loading are implemented: autoload on the slaves (\dQuote{slave}),
#'   autoload on the master (\dQuote{master}) or autoload on slave and master (\dQuote{both}).
#'   Exported objects will be assigned in the global environment.
#'   Default is \dQuote{slave} which mimics the export function in the snow package.
#' @return [character]. Invisibly returns the vector of exported object names.
#' @seealso \code{\link{batchUnexport}}, \code{\link{batchUnexport}}, \code{\link{batchImport}}
#' @export
batchExport = function(reg, ..., li=list(), trigger="slave") {
  checkArg(reg, cl="Registry")
  checkArg(li, "list")
  triggers = c("slave", "master", "both")
  checkArg(trigger, "character", choices=triggers)
  fd = getExportDir(reg$file.dir)

  objs = c(argsAsNamedList(...), li)
  if (!isProperlyNamed(objs))
    stop("Objects in '...' and/or 'li' are not properly named")

  # check if any of the objects already gets triggered in another directory
  for (alternative in setdiff(triggers, trigger)) {
    f = fail(file.path(fd, alternative))
    conflicting = intersect(names(objs), f$ls())
    if (length(conflicting))
      stopf("Object with name '%s' already triggered by '%s'", head(conflicting, 1L), alternative)
  }

  f = fail(file.path(fd, trigger))
  if (trigger != "slave") {
    keys = f$put(li = objs, use.cache=TRUE)
    f$assign(keys, envir=.GlobalEnv, use.cache=TRUE)
    f$clear()
  } else {
    keys = f$put(li = objs, use.cache=FALSE)
  }

  invisible(keys)
}

#' Import exported objects in the current R session.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param triggers [\code{character}]\cr
#'   Triggers from which to import. Subset of \code{c("both", "master", "slave")}.
#'   Default is \code{"slave"}.
#' @return [character]. Vector of names of imported objects.
#' @seealso \code{\link{batchExport}}, \code{\link{batchUnexport}}, \code{\link{batchListExported}}
#' @export
batchImport = function(reg, triggers = "slave") {
  checkArg(reg, "Registry")
  checkArg(triggers, subset=c("both", "master", "slave"))
  # FIXME assign does not return keys in current CRAN version!
  unlist(lapply(file.path(getExportDir(reg$file.dir), triggers),
                function(fd) fail(fd)$assign(envir = .GlobalEnv)))
}

#' List exported objects.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [list]. Returns a list of object names.
#'   The list is named according to the triggers in \code{\link{batchExport}}.
#' @seealso \code{\link{batchExport}}, \code{\link{batchUnexport}}, \code{\link{batchImport}}
#' @export
batchListExported = function(reg) {
  checkArg(reg, cl="Registry")
  fd = getExportDir(reg$file.dir)
  triggers = c("slave", "master", "both")
  sapply(triggers, function(trigger) {
    fail(file.path(fd, trigger))$ls()
  }, simplify=FALSE)
}

#' Unexport exported objects.
#'
#' Objects stored inside the registry's \code{file.dir} will be removed from the file system
#' which disables the automatic loading.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param keys [\code{\link{character}}]\cr
#'   Vector of object names as returned by \code{\link{batchListExported}} or returned by
#'   \code{\link{batchExport}}.
#' @return [character]. Invisibly returns a vector of unexported object names.
#' @seealso \code{\link{batchExport}}, \code{\link{batchListExported}}, \code{\link{batchImport}}
#' @export
batchUnexport = function(reg, keys) {
  checkArg(reg, cl="Registry")
  checkArg(keys, "character", na.ok=FALSE)
  fd = getExportDir(reg$file.dir)
  triggers = c("slave", "master", "both")

  res = lapply(triggers, function(trigger) {
    f = fail(file.path(fd, trigger))
    ok = f$remove(intersect(keys, f$ls()))
    names(ok)[ok]
  })
  invisible(unlist(res))
}
