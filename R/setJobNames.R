#' Set job names.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @param jobnames [\code{character}]\cr
#'   Character vector with length equal to \code{length(ids)}.
#'   \code{NA} removes the names stored in the registry.
#'   A single \code{NA} is replicated to match the length of ids provided.
#' @return Named vector of job ids.
#' @export
setJobNames = function(reg, ids, jobnames) {
  checkRegistry(reg, strict = TRUE)
  if (missing(ids))
    ids = dbGetJobIds(reg)
  else
    ids = checkIds(reg, ids)

  if (isScalarNA(jobnames))
    jobnames = rep.int(NA_character_, length(ids))
  else
    assertCharacter(jobnames, any.missing = FALSE)

  if (length(ids) != length(jobnames))
    stop("Arguments 'ids' and 'jobnames' must have same length")

  dbSetJobNames(reg, ids, jobnames)
  setNames(ids, jobnames)
}
