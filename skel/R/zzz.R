#' @import BBmisc

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  useBatchJobsConf()
}
