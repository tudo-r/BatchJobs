#' Loads a specific result file.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param missing.ok [\code{logical(1)}]\cr
#'   If \code{FALSE} an error is thrown if no result file is found.
#'   Otherwise \code{NULL} is returned.
#'   Default is \code{FALSE}.
#' @return [any]. Result of job.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResult = function(reg, id, part = NA_character_, missing.ok = FALSE) {
  checkRegistry(reg)
  syncRegistry(reg)
  id = checkId(reg, id)
  checkPart(reg, part)
  assertFlag(missing.ok)

  getResult(reg, id, part, missing.ok)
}

getResult = function(reg, id, part = NA_character_, missing.ok = FALSE) {
  getResults(reg, id, part, missing.ok)[[1L]]
}
