syncRegistry = function(reg) {
  conf = getBatchJobsConf()
  if (conf$staged.queries) {
    if (conf$debug && isOnSlave())
      stop("SQL query sent from Worker")

    fns = lsort(list.files(getPendingDir(reg$file.dir), full.names = TRUE))
    if (length(fns) == 0L)
      return(invisible(TRUE))

    message("Syncing registry ...")

    queries = lapply(fns, readSQLFile)
    ok = !vapply(queries, isFALSE, TRUE)
    tryCatch(dbDoQueries(reg, unlist(queries[ok]), "rw"),
             error = function(e) stopf("Error syncing registry (%s)", e))

    fns = fns[ok]
    ok = file.remove(fns)
    if (!all(ok))
      warningf("Some pending result sets could not be removed, e.g. '%s'", head(fns[ok], 1L))
  }

  invisible(TRUE)
}

readSQLFile = function(con) {
  x = try(readLines(con), silent=TRUE)
  n = length(x)
  if (is.error(x) || n == 0L || x[n] != "--EOF--")
    return(FALSE)
  x = x[-n]
  substr(x, 1L, nchar(x) - 1L)
}

writeSQLFile = function(x, con) {
  writeLines(c(paste0(x, ";"), "--EOF--"), con = con)
}

useStagedQueries = function() {
  getBatchJobsConf()$staged.queries
}

getOrderCharacters = function() {
  setNames(letters[1L:5L], c("submitted", "started", "done", "error", "killed"))
}
