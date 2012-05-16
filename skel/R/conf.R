# sources 1 config file and returns the envir
sourceConfFile = function(conffile) {
  checkArg(conffile, "character", len=1L, na.ok=FALSE)
  if (!file.exists(conffile)) {
    stopf("Configuration file does not exist: '%s'", conffile)
  }
  message("Sourcing configuration file: ", conffile)
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
readConfs = function() {
  fn.pack = file.path(.path.package("BatchJobs"), "etc", ".BatchJobs.R")
  fn.user = path.expand("~/.BatchJobs.R")
  fn.wd = suppressWarnings(normalizePath(".BatchJobs.R"))
  conffiles = Filter(file.exists, c(fn.pack, fn.user, fn.wd))
  if (length(conffiles) == 0L)
    stopf("No configuation found at all. Not in package, not in user.home, not in work dir!")
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
  conf$debug = FALSE
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

checkConf = function(conf) {
  ns = ls(conf)
  ns2 = c("cluster.functions", "mail.start", "mail.done", "mail.error",
    "mail.from", "mail.to", "mail.control", "db.driver", "db.options", "debug")
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following R variables in your config file:\n%s",
      collapse(ns2, sep=", "))
}

checkConfElements = function(cluster.functions, mail.to, mail.from,
  mail.start, mail.done, mail.error, mail.control,
  db.driver, db.options, debug) {

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
  if (!missing(debug))
    checkArg(debug, cl = "logical", len = 1L, na.ok = FALSE)
}

getClusterFunctions = function(conf) {
  conf$cluster.functions
}

#' Display BatchJobs configuration.
#' @return Nothing.
#' @export
showConf = function() {
  x = getBatchJobsConf()
  f = function(y) if(is.null(y)) "" else y
  catf("BatchJobs configuration:")
  catf("  cluster functions: %s", f(x$cluster.functions$name))
  catf("  mail.from: %s", f(x$mail.from))
  catf("  mail.to: %s", f(x$mail.to))
  catf("  mail.start: %s", f(x$mail.start))
  catf("  mail.done: %s", f(x$mail.done))
  catf("  mail.error: %s", f(x$mail.error))
  catf("  debug: %s", f(x$debug))
}
