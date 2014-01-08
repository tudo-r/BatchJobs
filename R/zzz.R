#' The BatchJobs package
#'
#' @section Additional information:
#'
#' \describe{
#'   \item{Homepage:}{\url{https://github.com/tudo-r/BatchJobs}}
#'   \item{Wiki:}{\url{https://github.com/tudo-r/BatchJobs/wiki}}
#'   \item{FAQ:}{\url{https://github.com/tudo-r/BatchJobs/wiki/FAQ}}
#'   \item{Configuration:}{\url{https://github.com/tudo-r/BatchJobs/wiki/Configuration}}
#' }
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
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
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
