#' @import BBmisc
#' @import utils
#' @import DBI
#' @import RSQLite
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom plyr rbind.fill

# FIXME should we set parent env to emptyenv() ?
.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  if (!isOnSlave()) {
    packageStartupMessage(collapse(capture.output(showConf()), "\n"))
  }
}

.onLoad = function(libname, pkgname) {
  if (!isOnSlave()) {
    assignConfDefaults()
    readConfs()
  }
}
