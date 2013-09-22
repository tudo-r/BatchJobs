#' BatchJobs configuration.
#'
#' In order to understand how the package should be configured
#' please read:
#' \url{https://code.google.com/p/batchjobs/wiki/Configuration}
#'
#' @name configuration
#' @rdname configuration
#' @aliases .BatchJobs.R
NULL

# sources 1 config file and returns the envir
sourceConfFile = function(conffile) {
  checkArg(conffile, "character", len=1L, na.ok=FALSE)
  if (!file.exists(conffile))
    stopf("Configuration file does not exist: '%s'", conffile)

  packageStartupMessage(sprintf("Sourcing configuration file: '%s'", conffile))
  conf = new.env()
  x = try(sys.source(conffile, envir=conf))
  if (is.error(x))
    stopf("There was an error in sourcing your configuration file '%s': %s!", conffile, as.character(x))
  checkConf(conf)
  do.call(checkConfElements, as.list(conf))
  return(conf)
}

# sources multiple config files, the later overwrite the first, and returns the envir
sourceConfFiles = function(conffiles) {
  conf = new.env()
  for (cf in conffiles) {
    conf2 = sourceConfFile(cf)
    lapply(ls(conf2), function(x) assign(x, conf2[[x]], envir=conf))
  }
  return(conf)
}

# assigns a conf to namespace
assignConf = function(conf) {
  conf.in.ns = getBatchJobsConf()
  lapply(ls(conf), function(x) assign(x, conf[[x]], envir=conf.in.ns))
}

# reads package conf, userhome conf, working dir conf
# then assigns them to namespace
readConfs = function(path) {
  fn.pack = file.path(path, "etc", "BatchJobs_global_config.R")
  fn.user = path.expand("~/.BatchJobs.R")
  fn.wd = suppressWarnings(normalizePath(".BatchJobs.R"))
  conffiles = Filter(file.exists, unique(c(fn.pack, fn.user, fn.wd)))
  if (length(conffiles) == 0L)
    stop("No configuation found at all. Not in package, not in user.home, not in work dir!")

  # really do this in 2 steps
  # otherwise weird things might happen due to lazy eval combined with envirs
  # and we might not see the error msg triggered in the checking of the config file
  conf = sourceConfFiles(conffiles)
  assignConf(conf)
}

assignConfDefaults = function() {
  conf = getBatchJobsConf()
  conf$cluster.functions = makeClusterFunctionsInteractive()
  conf$mail.start = "none"
  conf$mail.done = "none"
  conf$mail.error = "none"
  conf$db.driver = "SQLite"
  conf$db.options = list()
  conf$default.resources = list()
  conf$debug = FALSE
  conf$raise.warnings = FALSE
  conf$staged.queries = FALSE
  conf$max.concurrent.jobs = Inf
}

# loads conf into namespace on slave
loadConf = function(reg) {
  fn = getConfFilePath(reg)
  message("Loading conf: ", fn)
  ee = new.env()
  load(fn, envir=ee)
  ns = ls(ee$conf)
  # assign all stuff to conf in namespace
  conf = getBatchJobsConf()
  lapply(ns, function(x) assign(x, ee$conf[[x]], envir=conf))
  invisible(NULL)
}

getBatchJobsConf = function() {
  get(".BatchJobs.conf", envir=getNamespace("BatchJobs"))
}

saveConf = function(reg) {
  fn = getConfFilePath(reg)
  message("Saving conf: ", fn)
  conf = getBatchJobsConf()
  save(file=fn, conf)
}

getConfNames = function() {
  c("cluster.functions", "mail.start", "mail.done", "mail.error",
    "mail.from", "mail.to", "mail.control", "db.driver", "db.options",
    "default.resources", "debug", "raise.warnings", "staged.queries", "max.concurrent.jobs")
}

checkConf = function(conf) {
  ns = if (is.list(conf)) names(conf) else ls(conf, all.names = TRUE)
  ns2 = getConfNames()
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following R variables in your config:\n%s\nBut you also had:\n%s",
          collapse(ns2, sep=", "), collapse(setdiff(ns, ns2), sep=", "))
}

checkConfElements = function(cluster.functions, mail.to, mail.from,
  mail.start, mail.done, mail.error, mail.control, db.driver, db.options, default.resources, debug,
  raise.warnings, staged.queries, max.concurrent.jobs) {

  if (!missing(cluster.functions))
    checkArg(cluster.functions, cl = "ClusterFunctions")
  if (!missing(mail.from))
    checkArg(mail.from, cl = "character", len = 1L, na.ok = FALSE)
  if (!missing(mail.to))
    checkArg(mail.to, cl = "character", len = 1L, na.ok = FALSE)
  if (!missing(mail.start))
    checkArg(mail.start, choices = c("none", "first", "last", "first+last", "all"))
  if (!missing(mail.done))
    checkArg(mail.done, choices = c("none", "first", "last", "first+last", "all"))
  if (!missing(mail.error))
    checkArg(mail.error, choices = c("none", "first", "last", "first+last", "all"))
  if (!missing(mail.control))
    checkArg(mail.control, cl = "list")
  if (!missing(mail.control))
    checkArg(mail.control, cl = "list")
  if (!missing(mail.control))
    checkArg(mail.control, cl = "list")
  if (!missing(db.driver))
    checkArg(db.driver, cl = "character", len = 1L, na.ok = FALSE)
  if (!missing(db.options))
    checkArg(db.options, cl = "list")
  if (!missing(default.resources))
    checkArg(default.resources, cl = "list")
  if (!missing(debug))
    checkArg(debug, cl = "logical", len = 1L, na.ok = FALSE)
  if (!missing(raise.warnings))
    checkArg(raise.warnings, cl = "logical", len = 1L, na.ok = FALSE)
  if (!missing(staged.queries))
    checkArg(staged.queries, cl = "logical", len = 1L, na.ok = FALSE)
  if (!missing(max.concurrent.jobs))
    checkArg(max.concurrent.jobs, cl = "numeric", len = 1L, na.ok = FALSE)
}

getClusterFunctions = function(conf) {
  conf$cluster.functions
}

# Function which returns a printable string describing the config
# Used in packageStartupMessage and in print.Config
printableConf = function(conf) {
  x = as.list(conf)
  x[setdiff(getConfNames(), names(x))] = ""
  fmt = paste(
    "BatchJobs configuration:",
    "  cluster functions: %s",
    "  mail.from: %s",
    "  mail.to: %s",
    "  mail.start: %s",
    "  mail.done: %s",
    "  mail.error: %s",
    "  default.resources: %s",
    "  debug: %s",
    "  raise.warnings: %s",
    "  staged.queries: %s",
    "  max.concurrent.jobs: %s\n",
    sep = "\n")
  sprintf(fmt, x$cluster.functions$name, x$mail.from, x$mail.to, x$mail.start, x$mail.done,
          x$mail.error, listToShortString(x$default.resources), x$debug, x$raise.warnings,
          x$staged.queries, x$max.concurrent.jobs)
}


#' @S3method print Config
print.Config = function(x, ...) {
  cat(printableConf(x))
}

#' Load a specific configuration file.
#'
#' @param conffile [\code{character(1)}]\cr
#'   Location of the configuration file to load.
#'   Default is \dQuote{.BatchJobs.conf} in the current working directory.
#' @return Invisibly returns a list of configuration settings.
#' @seealso \code{\link{getConfig}}, \code{\link{setConfig}}
#' @export
loadConfig = function(conffile = ".BatchJobs.R") {
  # checks are done in sourceConfFile
  conf = sourceConfFile(conffile)
  assignConf(conf)
  invisible(setClasses(as.list(conf), "Config"))
}

#' Set and overwrite configuration settings
#'
#' @param conf [\code{Config} or \code{list}]\cr
#'   List of configuration parameters as returned by \code{\link{loadConfig}} or \code{\link{getConfig}}.
#' @param ... [\code{ANY}]\cr
#'   Named configuration parameters. Overwrites parameters in \code{conf}, if provided.
#' @return Invisibly returns a list of configuration settings.
#' @seealso \code{\link{getConfig}}, \code{\link{loadConfig}}
#' @export
setConfig = function(conf = list(), ...) {
  if (!is.list(conf) && !is(conf, "Config"))
    stopf("Argument 'conf' must be of class 'list' or 'Config', not %s", class(conf)[1])
  overwrites = insert(conf, list(...))
  if (!length(overwrites))
    return(invisible(getConfig()))
  if(! isProperlyNamed(overwrites))
    stopf("All configuration arguments in '...' must be properly named")
  checkConf(overwrites)
  conf = insert(as.list(getBatchJobsConf()), overwrites)
  assignConf(as.environment(conf))
  invisible(setClasses(conf, "Config"))
}

#' Returns a list of BatchJobs configuration settings
#'
#' @return \code{list} of current configuration variables with classs \dQuote{Config}.
#' @seealso \code{\link{loadConfig}}, \code{\link{setConfig}}
#' @export
getConfig = function() {
  setClasses(as.list(getBatchJobsConf()), "Config")
}
