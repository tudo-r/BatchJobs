#' @title Retrieve Job Parameters.
#'
#' @description
#' Returns parameters for all jobs as the rows of a data.frame.
#'
#' @template arg_reg
#' @template arg_ids
#' @return [\code{data.frame}]. Rows are named with job ids.
#' @export
#' @examples
#' # see batchExpandGrid
getJobParamDf = function(reg, ids) {
  checkRegistry(reg, strict = TRUE, writeable = FALSE)
  syncRegistry(reg)
  if (!missing(ids))
    ids = checkIds(reg, ids)
  tab = dbGetExpandedJobsTable(reg, ids, cols = c("job_id", "pars"))
  res = deserialize(tab$pars)
  setDF(res, rownames = rownames(tab))

  return(res)
}
