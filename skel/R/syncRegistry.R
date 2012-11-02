syncRegistry = function(reg) { # FIXME option to use external sync
  if (useStagedQueries()) {
    stopifnot(isOnSlave == FALSE)
    path = getPendingDir(reg$file.dir)
    fns = file.path(path, lsort(list.files(path)))
    if (length(fns) == 0L)
      return(invisible(TRUE))

    message("Syncing registry ...")

    queries = lapply(fns, readSQLFile)
    ok = !vapply(queries, isFALSE, TRUE)
    fns = fns[ok]

    tryCatch(dbDoQueries(reg, unlist(queries[ok]), "rw"),
             error = function(e) stopf("Error syncing registry (%s)", e))

    ok = file.remove(fns)
    if (!all(ok))
      warningf("Some pending result sets could not be removed, e.g. '%s'", head(fns[ok], 1L))
  }

  invisible(TRUE)
}

readSQLFile = function(con) {
  x = readLines(con)
  n = length(x)
  if (n <= 1L || x[n] != "--EOF--")
    return(FALSE)
  x = x[-n]
  substr(x, 1L, nchar(x) - 1L)
}

writeSQLFile = function(x, con) {
  writeLines(c(paste(x, ";"), "--EOF--"), con = con)
}

useStagedQueries = function() {
  getBatchJobsConf()$staged.queries
}

getOrderCharacters = function() {
  setNames(c("a", "b", "c", "d", "e"),
           c("submitted", "started", "done", "error", "killed"))
}
