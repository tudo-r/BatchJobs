#' @import BBmisc
#' @import utils
#' @import DBI
#' @import RSQLite
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom plyr rbind.fill

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  if (!isOnSlave()) {
    packageStartupMessage(collapse(capture.output(showConf()), "\n"))
  }
}

.onLoad = function(libname, pkgname) {
  if (!isOnSlave()) {
    assignConfDefaults()
    if (missing(libname) || missing(pkgname)) {
      # this can happen with testthat while loading from skel/
      readConfs(find.package(package = "BatchJobs"))
    } else {
      readConfs(file.path(libname, pkgname))
    }
  }
}
