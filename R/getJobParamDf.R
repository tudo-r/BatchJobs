#' @title Returns parameters for all jobs as the rows of a data.frame.
#'
#' @template arg_reg
#' @template arg_ids
#' @return [\code{data.frame}]. Rows are named with job ids.
#' @export
#' @examples
#' # see batchExpandGrid
getJobParamDf = function(reg, ids) {
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  tab = dbGetExpandedJobsTable(reg, ids, cols = c("job_id", "pars"))
  # rownames are set by db* call
  # unserialize parameters
  res = convertListOfRowsToDataFrame(lapply(tab$pars,
      function(x) unserialize(charToRaw(x))), row.names = rownames(tab))
  return(dropNamed(res, "job_id"))
}
