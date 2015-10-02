#' Manually query the BatchJobs database
#'
#' @template arg_reg
#' @param query [\code{character(1)}]\cr
#'   SQL query to send to the database.
#' @param flags [\code{character(1)}]\cr
#'   One of \dQuote{ro}, \dQuote{rw} or \dQuote{rwc} which is translated
#'   to \code{SQLITE_RO}, \code{SQLITE_RW} or \code{SQLITE_RWC}, respectively.
#'   See \link[RSQLite]{SQLITE_RO} for more info.
#' @return [\code{data.frame}] Result of the query.
#' @export
#' @examples
#' reg = makeRegistry("test", file.dir = tempfile())
#' batchMap(reg, identity, i = 1:10)
#' batchQuery(reg, "SELECT * FROM test_job_status")
batchQuery = function(reg, query, flags = "ro") {
  assertString(query)
  assertChoice(flags, c("ro", "rw", "rwc"))
  dbDoQuery(reg, query = query, flags = flags, max.retries = 3L)
}
