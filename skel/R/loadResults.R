#' Loads result files for id vector.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all done jobs.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified to a vector, matrix or higher dimensional array if possible?
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should the returned list be named with the ids?
#'   Default is \code{TRUE}.
#' @return [\code{list}]. Results of jobs as list, possibly named by ids.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResults = function(reg, ids, part=NA_character_, simplify=FALSE, use.names=TRUE) {
  checkArg(reg, "Registry")
  if (missing(ids)) {
    ids = dbGetDone(reg)
  } else {
    ids = checkIds(reg, ids)
  }
  checkArg(simplify, "logical", len=1L, na.ok=FALSE)
  checkArg(use.names, "logical", len=1L, na.ok=FALSE)

  res = lapply(ids, loadResult, reg=reg, part=part, check.id=FALSE)
  if(use.names)
    names(res) = ids
  if(simplify && length(res) > 0L)
    res = simplify2array(res, higher = (simplify=="array"))

  return(res)
}
