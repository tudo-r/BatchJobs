#' Loads result files for id vector.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all done jobs.
#' @param part [\code{character(1)}]
#'   Only useful for multiple result files, then defines which result file part should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param simplify [\code{logical(1)}]\cr
#'   Should the result be simplified to a vector, matrix or higher dimensional array if possible?
#'   Default is \code{TRUE}.
#' @param use.names [\code{logical(1)}]\cr
#'   Should the returned list be named with the ids?
#'   Default is \code{TRUE}.
#' @param check.ids [\code{logical(1)}]\cr
#'   Check the job ids?
#'   Default is \code{TRUE}.
#' @return [\code{list}]. Results of jobs as list, possibly named by ids.
#' @seealso \code{\link{reduceResults}}
#' @export
loadResults = function(reg, ids, part=as.character(NA), simplify=FALSE, use.names=TRUE, check.ids=TRUE) {
  checkArg(reg, "Registry")
  if (missing(ids)) {
    ids = dbGetDone(reg)
  } else if(check.ids) {
      ids = convertIntegers(ids)
      checkArg(ids, "integer", na.ok=FALSE)
      checkIds(reg, ids)
  }
  checkArg(simplify, "logical", len=1, na.ok=FALSE)
  checkArg(use.names, "logical", len=1, na.ok=FALSE)
  checkArg(check.ids, "logical", len=1, na.ok=FALSE)
  res = sapply(ids, loadResult, reg=reg, part=part, check.id=FALSE,
               simplify=simplify, USE.NAMES=FALSE)
  if (use.names)
    names(res) = ids
  return(res)
}
