#' Loads a specific result file.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{integer(1)}]\cr
#'   Id of job.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @return [any]. Result of job.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResult = function(reg, id, part=NA_character_) {
  checkRegistry(reg)
  syncRegistry(reg)
  id = checkId(reg, id)
  checkPart(reg, part)

  getResult(reg, id, part)
}

getResult = function(reg, id, part=NA_character_) {
  getResults(reg, id, part)[[1L]]
}
