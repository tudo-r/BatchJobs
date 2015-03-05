resrc = function(res.new) {
  res.old = getBatchJobsConf()$default.resources
  if(!isProperlyNamed(res.new) || !isProperlyNamed(res.old))
    stop("Resources must be all be uniquely named!")
  insert(res.old, res.new)
}

saveResources = function(reg, resources, timestamp = now()) {
  fn = getResourcesFilePath(reg, timestamp)
  save2(file = fn, resources = resources)
  return(timestamp)
}

#' Function to get job resources in job function.
#'
#' Return the list passed to \code{\link{submitJobs}}, e.g.
#' nodes, walltime, etc.
#'
#' Can only be called in job function during job execution on slave.
#'
#' @return [\code{list}].
#' @export
getResources = function() {
  if (!isOnSlave())
    stop("getResources can only be called during job execution on slave!")
  load2(getOption("BatchJobs.resources.path"))
}

#' Function to get the resources that were submitted for some jobs.
#'
#' Throws an error if call it for unsubmitted jobs.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all submitted jobs.
#' @param as.list [\code{integer(1)}]\cr
#'   If \code{FALSE} a data.frame will be returned.
#'   Default is \code{TRUE}.
#' @return [\code{list} | \code{data.frame}]. List (or data.frame) of resource lists as passed to \code{\link{submitJobs}}.
#' @export
getJobResources = function(reg, ids, as.list = TRUE) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindSubmitted(reg)
  } else {
    ids = checkIds(reg, ids)
    nsub = dbFindSubmitted(reg, ids, negate = TRUE, limit = 1L)
    if (length(nsub) > 0L)
      stopf("Some of your jobs have not been submitted, so no resources are available, e.g. for id=%i", nsub)
  }
  query = sprintf("SELECT job_id, resources_timestamp FROM %s_job_status", reg$id)
  df = dbSelectWithIds(reg, query, ids)
  res = namedList(df$job_id)
  for(ts in unique(df$resources_timestamp)) {
    res[df$resources_timestamp == ts] = load2(getResourcesFilePath(reg, ts), simplify = FALSE)
  }
  if (!as.list)
    res = convertListOfRowsToDataFrame(res)
  return(res)
}
