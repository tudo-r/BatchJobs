#' Get execution times of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param unit [\code{character}]\cr
#'   Unit to convert execution times to.
#'   Possible values: \dQuote{seconds}, \dQuote{minutes}, \dQuote{hours},
#'   \dQuote{days} and \dQuote{weeks}.
#'   Default is \dQuote{seconds}.
#' @return [\code{numeric}]. Time in \code{unit} for jobs, named with ids.
#'   \code{NA} if job has not terminated successfully.
#' @export
getJobTimes = function(reg, ids, unit = "seconds") {
  checkArg(reg, cl = "Registry")
  if (missing(ids))
    ids = dbGetJobIds(reg)
  else
    ids = checkIds(reg, ids)
  conv = setNames(c(1L, 60L, 3600L, 86400L, 604800L),
                  c("seconds", "minutes", "hours", "days", "weeks"))
  checkArg(unit, choices = names(conv))
  setNames(dbGetJobTimes(reg, ids)$time / conv[unit], ids)
}
