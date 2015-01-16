checkIds = function(reg, ids, check.present = TRUE) {
  ids = asInteger(ids, any.missing = FALSE, unique = TRUE)
  if (check.present)
    dbCheckJobIds(reg, ids)
  return(ids)
}

checkId = function(reg, id, check.present = TRUE) {
  id = asInt(id)
  if (check.present)
    dbCheckJobIds(reg, id)
  return(id)
}

checkMoreArgs = function(more.args, reserved) {
  assertList(more.args, names = "strict")
  n = names(more.args)
  if(is.null(n) || missing(reserved))
    return(invisible(TRUE))

  check = reserved %in% n
  if (any(check))
    stopf("more.args uses element names which are internally reserved: %s",
          collapse(reserved[check]))
  return(invisible(TRUE))
}

checkPart = function(reg, part) {
  if (reg$multiple.result.files) {
    if (!testScalarNA(part) && !testCharacter(part, any.missing = FALSE))
      stop("'part' must be NA or a character vector without NAs!")
  } else {
    if (!testScalarNA(part))
      stop("'part' must be NA because multiple.result.files is FALSE!")
  }
}


getListJobs = function(msg = NULL) {
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  fun = cf$listJobs
  if (is.null(fun) && !is.null(msg))
    stopf("%s because %s cluster functions do not support listing of jobs!", msg, cf$name)
  return(fun)
}

getKillJob = function(msg = NULL) {
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  fun = cf$killJob
  if (is.null(fun) && !is.null(msg))
    stopf("%s because %s cluster functions do not support killing of jobs!", msg, cf$name)
  return(fun)
}

getBatchIds = function(reg, msg = NULL) {
  fun = getListJobs(msg)
  fun(getBatchJobsConf(), reg)
}

getRandomSeed = function(n = 1L) {
  as.integer(runif(n, 1, .Machine$integer.max / 2L))
}

seeder = function(reg, seed) {
  if(!exists(".Random.seed", envir = .GlobalEnv))
     runif(1L)
  prev.seed = get(".Random.seed", envir = .GlobalEnv)
  prev.kind = RNGkind()
  set.seed(seed, kind = reg$RNGkind[1L], normal.kind = reg$RNGkind[2L])

  return(list(
    reset = function() {
      RNGkind(kind = prev.kind[1L], normal.kind = prev.kind[2L])
      assign(".Random.seed", prev.seed, envir = .GlobalEnv)
    }))
}

switchWd = function(reg) {
  cur = getwd()
  message("Setting work dir: ", reg$work.dir)
  setwd(reg$work.dir)

  return(list(reset = function() {
    message("Setting work back to: ", cur)
    setwd(cur)
  }))
}

addIntModulo = function(x, y, mod = .Machine$integer.max) {
  as.integer((as.double(x) + as.double(y)) %% mod)
}

isOnSlave = function() {
  getOption("BatchJobs.on.slave", default = FALSE)
}

setOnSlave = function(x, resources.path = as.character(NA)) {
  options(BatchJobs.on.slave = x)
  options(BatchJobs.resources.path = resources.path)
}

now = function() {
  as.integer(Sys.time())
}

getArgNames = function(args) {
  if (!length(args))
    return(NULL)
  if (is.null(names(args[[1L]])) && is.character(args[[1L]]))
      return(args[[1L]])
  return(names(args[[1L]]))
}

convertUseNames = function(use.names) {
  if (is.character(use.names) && length(use.names) == 1L && use.names %in% c("none", "ids", "names"))
    return(use.names)
  assertFlag(use.names)
  c("none", "ids")[use.names+1L]
}

waitForFiles = function(fn, timeout = NA_real_, sleep = 1) {
  if (is.na(timeout))
    return(invisible(TRUE))

  fn = fn[!file.exists(fn)]
  if (length(fn)) {
    start = now()
    repeat {
      Sys.sleep(sleep)
      fn = fn[!file.exists(fn)]
      if (!length(fn))
        break
      if (now() - start > timeout)
        stopf("Error waiting for file system. File '%s' timed out after %.1f seconds", head(fn, 1L), timeout)
    }
  }

  invisible(TRUE)
}

info = function(...) {
  if (getOption("BatchJobs.verbose", default = TRUE))
    message(sprintf(...))
}

getProgressBar = function(condition, ...) {
  if (condition) {
    pb = makeProgressBar(...)
    pb$set()
  } else {
    pb = makeProgressBar(style = "off")
  }
  pb
}

checkUserFunction = function(fun) {
  fun = match.fun(fun)
  if (getOption("BatchJobs.clear.function.env")) {
    environment(fun) = .GlobalEnv
  } else {
    ee = environment(fun)
    if (!is.null(ee) && !isNamespace(ee)) {
      nn = ls(ee, all.names = TRUE)
      if (sum(vnapply(nn, function(nn) object.size(ee[[nn]])) / 1024^2) > 10)
        warning("The environment of provided function exceeds 10Mb.")
    }
  }
  fun
}
