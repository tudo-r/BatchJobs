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
#' @import checkmate
#' @import utils
#' @import stats
#' @import DBI
#' @import RSQLite
#' @import fail
#' @import methods
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
NULL

.BatchJobs.conf = new.env()

.onAttach = function(libname, pkgname) {
  if (!isOnSlave()) {
    if (missing(libname) || missing(pkgname)) {
      # this can happen with testthat while loading from skel/
      readConfs(find.package(package = "BatchJobs"))
    } else {
      readConfs(file.path(libname, pkgname))
    }
    if (getOption("BatchJobs.verbose", default = TRUE))
      packageStartupMessage(printableConf(getConfig()))
  }
}

.onLoad = function(libname, pkgname) {
# checking for posix might create problem in windwos tests
  options(BatchJobs.check.posix = getOption("BatchJobs.check.posix", default = !isWindows()))
  if (!isOnSlave()) {
    assignConfDefaults()
  }
}
