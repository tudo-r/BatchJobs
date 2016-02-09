#' Loads a specific result file.
#' @template arg_reg
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param missing.ok [\code{logical(1)}]\cr
#'   If \code{FALSE} an error is thrown if no result file is found.
#'   Otherwise \code{NULL} is returned.
#'   Default is \code{FALSE}.
#' @param impute.val [any]\cr
#'   The value to return when no result is available.
#'   Defaults to \code{NULL} (the previous behavior)
#' @return [any]. Result of job.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResult = function(reg, id, part = NA_character_, missing.ok = FALSE, impute.val = NULL) {
  checkRegistry(reg, writeable = FALSE)
  syncRegistry(reg)
  id = checkIds(reg, id, len = 1L)
  checkPart(reg, part)
  assertFlag(missing.ok)

  getResult(reg, id, part, missing.ok, impute.val)
}

getResult = function(reg, id, part = NA_character_, missing.ok = FALSE, impute.val = NULL) {
  getResults(reg, id, part, missing.ok, impute.val)[[1L]]
}
