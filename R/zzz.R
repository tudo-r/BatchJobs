#' The BatchJobs package
#'
#' @section Additional information:
#'
#' Wiki, FAQ, examples and more on \url{https://github.com/tudo-r/BatchJobs}.
#'
#' @docType package
#' @name BatchJobs
NULL

#' @import BBmisc
#' @import utils
#' @import DBI
#' @import RSQLite
#' @import fail
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom plyr rbind.fill
NULL

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  if (!isOnSlave()) {
    if (missing(libname) || missing(pkgname)) {
      # this can happen with testthat while loading from skel/
      readConfs(find.package(package = "BatchJobs"))
    } else {
      readConfs(file.path(libname, pkgname))
    }
    packageStartupMessage(printableConf(getConfig()))
  }
}

.onLoad = function(libname, pkgname) {
  options(BatchJobs.check.posix = getOption("BatchJobs.check.posix", default = TRUE))
  if (!isOnSlave()) {
    assignConfDefaults()
  }
}
