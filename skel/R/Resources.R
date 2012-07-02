resrc = function(...) {
  res1 = getBatchJobsConf()$default.resources
  res2 = list(...)
  if(!isProperlyNamed(res1) || !isProperlyNamed(res2))
    stop("Resources must be all be uniquely named!")
  res1 = insert(res1, res2)
  return(res1)
}

saveResources = function(reg, resources, timestamp = as.integer(Sys.time())) {
  fn = getResourcesFilePath(reg, timestamp)
  save2(file=fn, resources=resources)
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
#'   If \code{FALSE}, \code{rbind.fill} is called on the result to
#'   convert it to a data.frame.
#'   Default is \code{TRUE}.
#' @return [\code{list} | \code{data.frame}]. List (or data.frame) of resource lists as passed to \code{\link{submitJobs}}.
#' @export
getJobResources = function(reg, ids, as.list=TRUE) {  
  checkArg(reg, "Registry")
  if (missing(ids)) {
    ids = findSubmitted(reg)
  } else {
    ids = checkIds(reg, ids)
    nsub = ids[ids %nin% findSubmitted(reg)]
    if (length(nsub > 0))
      stopf("Some of your jobs have not been submitted, so no resources are available, e.g. for id=%i", nsub[1])
  } 
  query = sprintf("SELECT job_id, resources_timestamp FROM %s_job_status", reg$id)
  df = dbSelectWithIds(reg, query, ids)
  res = namedList(df$job_id)
  for(ts in unique(df$resources_timestamp)) {
    res[df$resources_timestamp == ts] = load2(getResourcesFilePath(reg, ts), simplify=FALSE)
  }         
  if (!as.list)
    res = do.call(rbind.fill, lapply(res, as.data.frame))
  return(res)              
}
