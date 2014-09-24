#' Show information about available computational resources on cluster.
#'
#' @description
#' Currently only supported for multicore and SSH mode.
#' Displays: Name of node, current load, number of running R processes, number of R processes
#' with more than 50% load, number of BatchJobs jobs running.
#' The latter counts either jobs belonging to \code{reg} or all BatchJobs jobs if reg was not passed.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#'   Must not be passed and this is the default.
#' @return [\code{data.frame}]. Returns the displayed table invisibly.
#' @export
# FIXME: allow to get a quick overview by passing nodenames
showClusterStatus = function(reg) {
  if (missing(reg)) {
    file.dir = ""
  } else {
    checkRegistry(reg)
    syncRegistry(reg)
    file.dir = reg$file.dir
  }
  conf = getBatchJobsConf()
  cf = conf$cluster.functions
  if (cf$name %nin% c("Multicore", "SSH"))
    stop("showWorkerStatus can only be used in multicore or SSH mode!")
  workers = environment(cf$submitJob)$workers
  data = lapply(workers, getWorkerStatus, file.dir = file.dir)
  data = do.call(rbind, lapply(data, as.data.frame))
  data = cbind(ncpus = extractSubList(workers, "ncpus"), data)
  print(data)
  invisible(data)
}
