#' @title The BatchJobs package
#'
#' @description
#' Provides Map, Reduce and Filter variants to generate jobs on batch
#' computing systems like PBS/Torque, LSF, SLURM and Sun Grid Engine.
#' Multicore and SSH systems are also supported. For further details see the
#' project web page.
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
#' @name BatchJobs
"_PACKAGE"

#' @import utils
#' @import stats
#' @import methods
#' @import checkmate
#' @import data.table
#' @import DBI
#' @import RSQLite
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom stringi stri_extract_first_regex
#' @importFrom stringi stri_trim_both
#' @importFrom stringi stri_split_fixed
#' @importFrom stringi stri_split_regex
#' @importFrom BBmisc %nin%
#' @importFrom BBmisc chunk
#' @importFrom BBmisc checkListElementClass
#' @importFrom BBmisc clipString
#' @importFrom BBmisc collapse
#' @importFrom BBmisc convertToShortString
#' @importFrom BBmisc convertListOfRowsToDataFrame
#' @importFrom BBmisc dropNamed
#' @importFrom BBmisc extractSubList
#' @importFrom BBmisc filterNull
#' @importFrom BBmisc insert
#' @importFrom BBmisc isDirectory
#' @importFrom BBmisc is.error
#' @importFrom BBmisc isProperlyNamed
#' @importFrom BBmisc isScalarNA
#' @importFrom BBmisc isWindows
#' @importFrom BBmisc lsort
#' @importFrom BBmisc namedList
#' @importFrom BBmisc names2
#' @importFrom BBmisc makeFileCache
#' @importFrom BBmisc makeProgressBar
#' @importFrom BBmisc makeSimpleFileLogger
#' @importFrom BBmisc save2 load2
#' @importFrom BBmisc requirePackages
#' @importFrom BBmisc setClasses setColNames setRowNames
#' @importFrom BBmisc seq_col seq_row
#' @importFrom BBmisc suppressAll
#' @importFrom BBmisc system3
#' @importFrom BBmisc vcapply viapply vlapply vnapply
#' @importFrom BBmisc warningf stopf messagef catf
#' @importFrom BBmisc which.first
NULL

.BatchJobs.conf = new.env(parent = emptyenv())
.BatchJobs.conf$cluster.functions = makeClusterFunctionsInteractive()
.BatchJobs.conf$mail.start = "none"
.BatchJobs.conf$mail.done = "none"
.BatchJobs.conf$mail.error = "none"
.BatchJobs.conf$db.driver = "SQLite"
.BatchJobs.conf$db.options = list(pragmas = "busy_timeout=5000")
.BatchJobs.conf$default.resources = list()
.BatchJobs.conf$debug = FALSE
.BatchJobs.conf$raise.warnings = FALSE
.BatchJobs.conf$staged.queries = TRUE
.BatchJobs.conf$max.concurrent.jobs = Inf
.BatchJobs.conf$fs.timeout = NA_real_
.BatchJobs.conf$measure.mem = TRUE

.BatchJobs.conffiles = character(0L)

.onAttach = function(libname, pkgname) {
  packageStartupMessage("The development of BatchJobs and BatchExperiments is discontinued.")
  packageStartupMessage("Consider switching to 'batchtools' for new features and improved stability")
  if (getOption("BatchJobs.verbose", default = TRUE)) {
    cf = .BatchJobs.conffiles
    packageStartupMessage(sprintf("Sourced %i configuration files: ", length(cf)))
    for (i in seq_along(cf))
      packageStartupMessage(sprintf("  %i: %s", i, cf[i]))
    conf = getConfig()
    packageStartupMessage(printableConf(conf))
  }
}

.onLoad = function(libname, pkgname) {
  options(BatchJobs.check.posix = getOption("BatchJobs.check.posix", default = !isWindows()))
  options(BatchJobs.clear.function.env = getOption("BatchJobs.clear.function.env", default = FALSE))
  backports::import(pkgname)

  if (!isOnSlave()) {
    if (getOption("BatchJobs.load.config", TRUE)) {
      pkg = if(missing(libname) || missing(pkgname)) find.package(package = "BatchJobs") else file.path(libname, pkgname)
      .BatchJobs.conffiles <<- readConfs(pkg)
    }
  }
}
