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
#' The package currently support the following further R options, which you can set
#' either in your R profile file or a script via \code{\link{options}}:
#'
#' \describe{
#'   \item{BatchJobs.verbose}{This boolean flag can be set to \code{FALSE} to reduce the
#'     console output of the package operations. Usually you want to see this output in interactive
#'     work, but when you  use the package in e.g. knitr documents,
#'     it clutters the resulting document too much.}
#'   \item{BatchJobs.check.posix}{If this boolean flag is enabled, the package checks your
#'     registry file dir (and related user-defined directories) quite strictly to be POSIX compliant.
#'     Usually this is a good idea, you do not want to have strange chars in your file paths,
#'     as this might results in problems  when these paths get passed to the scheduler or other
#'     command-line tools that the package interoperates with.
#'     But on some OS this check might be too strict and cause problems.
#'     Setting the flag to \code{FALSE} allows to disable the check entirely.
#'     The default is \code{FALSE} on Windows systems and \code{TRUE} else.}
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
  if (getOption("BatchJobs.load.config", TRUE) && !isOnSlave()) {
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
  options(BatchJobs.clear.function.env = getOption("BatchJobs.clear.function.env", default = FALSE))
  if (!isOnSlave()) {
    assignConfDefaults()
  }
}
