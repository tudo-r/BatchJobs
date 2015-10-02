#' Syncronize staged queries into the registry.
#'
#' @description
#' If the option \dQuote{staged.queries} is enabled, all communication from the nodes
#' to the master is done via files in the subdirectory \dQuote{pending} of the \code{file.dir}.
#' This function checks for such files and merges the information into the database.
#' Usually you do not have to call this function yourself.
#'
#' @template arg_reg
#' @return Invisibly returns \code{TRUE} on success.
#' @export
syncRegistry = function(reg) {
  conf = getBatchJobsConf()
  if (conf$staged.queries) {
    if (conf$debug && isOnSlave())
      stop("SQL query sent from Worker")

    fns = lsort(list.files(getPendingDir(reg$file.dir), full.names = TRUE))
    if (length(fns) == 0L)
      return(invisible(TRUE))

    info("Syncing registry ...")

    queries = lapply(fns, readSQLFile)
    ok = !vlapply(queries, isFALSE)
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
  x = try(readLines(con), silent = TRUE)
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

.OrderChars = setNames(letters[1L:6L], c("first", "submitted", "started", "done", "error", "last"))
