#' BatchJobs configuration.
#'
#' In order to understand how the package should be configured
#' please read
#' \url{https://github.com/tudo-r/BatchJobs/wiki/Configuration}.
#'
#' @name configuration
#' @rdname configuration
#' @family conf
#' @aliases .BatchJobs.R
NULL

# sources 1 config file and returns the envir
sourceConfFile = function(conffile) {
  assertFile(conffile)

  if (getOption("BatchJobs.verbose", default = TRUE))
    packageStartupMessage(sprintf("Sourcing configuration file: '%s'", conffile))
  conf = new.env()
  x = try(sys.source(conffile, envir = conf))
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
    lapply(ls(conf2), function(x) assign(x, conf2[[x]], envir = conf))
  }
  return(conf)
}

# assigns a conf to namespace
assignConf = function(conf) {
  conf.in.ns = getBatchJobsConf()
  lapply(ls(conf), function(x) assign(x, conf[[x]], envir = conf.in.ns))
}

# locates package conf, userhome conf, working dir conf
findConfigs = function(path=find.package("BatchJobs")) {
  fn.pack = file.path(path, "etc", "BatchJobs_global_config.R")
  fn.user = path.expand("~/.BatchJobs.R")
  fn.wd = suppressWarnings(normalizePath(".BatchJobs.R"))
  Filter(file.exists, unique(c(fn.pack, fn.user, fn.wd)))
}

# reads available config files and assigns them to namespace
readConfs = function(path=find.package("BatchJobs")) {
  conffiles = findConfigs(path)
  if (length(conffiles) == 0L) {
    warning("No configuation found at all. Not in package, not in user.home, not in work dir!")
    assignConfDefaults()
  }

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
  conf$db.options = list(pragmas = "busy_timeout=5000")
  conf$default.resources = list()
  conf$debug = FALSE
  conf$raise.warnings = FALSE
  conf$staged.queries = TRUE
  conf$max.concurrent.jobs = Inf
  conf$fs.timeout = NA_real_
}

# loads conf into namespace on slave
loadConf = function(reg) {
  fn = getConfFilePath(reg)
  info("Loading conf: ", fn)
  ee = new.env()
  load(fn, envir = ee)
  ns = ls(ee$conf)
  # assign all stuff to conf in namespace
  conf = getBatchJobsConf()
  lapply(ns, function(x) assign(x, ee$conf[[x]], envir = conf))
  invisible(NULL)
}

getBatchJobsConf = function() {
  get(".BatchJobs.conf", envir = getNamespace("BatchJobs"))
}

saveConf = function(reg) {
  fn = getConfFilePath(reg)
  info("Saving conf: %s", fn)
  conf = getBatchJobsConf()
  save(file = fn, conf)
}

getConfNames = function() {
  c("cluster.functions", "mail.start", "mail.done", "mail.error",
    "mail.from", "mail.to", "mail.control", "db.driver", "db.options",
    "default.resources", "debug", "raise.warnings", "staged.queries",
    "max.concurrent.jobs", "fs.timeout")
}

checkConf = function(conf) {
  ns = if (is.list(conf)) names(conf) else ls(conf, all.names = TRUE)
  ns2 = getConfNames()
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following R variables in your config:\n%s\nBut you also had:\n%s",
          collapse(ns2, sep = ", "), collapse(setdiff(ns, ns2), sep = ", "))
}

checkConfElements = function(cluster.functions, mail.to, mail.from,
  mail.start, mail.done, mail.error, mail.control, db.driver, db.options, default.resources, debug,
  raise.warnings, staged.queries, max.concurrent.jobs, fs.timeout) {

  mail.choices = c("none", "first", "last", "first+last", "all")

  if (!missing(cluster.functions))
    assertClass(cluster.functions, "ClusterFunctions")
  if (!missing(mail.from))
    assertString(mail.from)
  if (!missing(mail.to))
    assertString(mail.to)
  if (!missing(mail.start))
    assertChoice(mail.start, mail.choices)
  if (!missing(mail.done))
    assertChoice(mail.done, mail.choices)
  if (!missing(mail.error))
    assertChoice(mail.error, mail.choices)
  if (!missing(mail.control))
    assertList(mail.control)
  if (!missing(db.driver))
    assertString(db.driver)
  if (!missing(db.options))
    assertList(db.options, names = "named")
  if (!missing(default.resources))
    assertList(default.resources, names = "named")
  if (!missing(debug))
    assertFlag(debug)
  if (!missing(raise.warnings))
    assertFlag(raise.warnings)
  if (!missing(staged.queries))
    assertFlag(staged.queries)
  if (!missing(max.concurrent.jobs))
    assertCount(max.concurrent.jobs)
  if (!missing(fs.timeout))
    assertNumber(fs.timeout)
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
    "  max.concurrent.jobs: %s",
    "  fs.timeout: %s\n",
    sep = "\n")
  sprintf(fmt, x$cluster.functions$name, x$mail.from, x$mail.to, x$mail.start, x$mail.done,
    x$mail.error, convertToShortString(x$default.resources), x$debug, x$raise.warnings,
    x$staged.queries, x$max.concurrent.jobs, x$fs.timeout)
}


#' @export
print.Config = function(x, ...) {
  cat(printableConf(x))
}

#' Load a specific configuration file.
#'
#' @param conffile [\code{character(1)}]\cr
#'   Location of the configuration file to load.
#'   Default is \dQuote{.BatchJobs.conf} in the current working directory.
#' @return Invisibly returns a list of configuration settings.
#' @family conf
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
#' @family conf
#' @export
setConfig = function(conf = list(), ...) {
  if (!is.list(conf) && !inherits(conf, "Config"))
    stopf("Argument 'conf' must be of class 'list' or 'Config', not %s", head(conf, 1L))
  overwrites = insert(conf, list(...))
  if (length(overwrites) == 0L)
    return(invisible(getConfig()))
  if (!isProperlyNamed(overwrites))
    stopf("All configuration arguments in '...' must be properly named")
  checkConf(overwrites)
  conf = insert(as.list(getBatchJobsConf()), overwrites)
  assignConf(as.environment(conf))
  invisible(setClasses(conf, "Config"))
}

#' Returns a list of BatchJobs configuration settings
#'
#' @return \code{list} of current configuration variables with classs \dQuote{Config}.
#' @family conf
#' @export
getConfig = function() {
  setClasses(as.list(getBatchJobsConf()), "Config")
}
