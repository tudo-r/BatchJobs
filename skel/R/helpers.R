checkIds = function(reg, ids) {
  if (anyDuplicated(ids) > 0L) {
    dup = ids[duplicated(ids)]
    stopf("You have duplicated entries in your id vector: %s", collapse(dup))
  }
  ids = setdiff(ids, dbGetJobIds(reg))
  if (length(ids) > 0L)
    stopf("Ids not present in registry: %s", collapse(ids))
}

checkMoreArgs = function(more.args, reserved) {
  checkArg(more.args, cl="list")
  n = names(more.args)
  if(is.null(n) || missing(reserved))
    return(invisible(TRUE))

  check = reserved %in% n
  if (any(check))
    stopf("more.args uses element names which are internally reserved: %s",
          collapse(reserved[check]))
  return(invisible(TRUE))
}

getListJobs = function(msg=NULL) {
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  fun = cf$listJobs
  if (is.null(fun))
    if (!is.null(msg))
      stopf("%s because %s cluster functions do not support listing of jobs!", msg, cf$name)
  return(fun)
}

getKillJob = function(msg=NULL) {
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  fun = cf$killJob
  if (is.null(fun))
    if (!is.null(msg))
      stopf("%s because %s cluster functions do not support killing of jobs!", msg, cf$name)
  return(fun)
}

getRandomSeed = function(n = 1L) {
  as.integer(runif(n, 1, floor(.Machine$integer.max / 2L)))
}

seeder = function(reg, seed) {
  if(!exists(".Random.seed", envir = .GlobalEnv))
     runif(1L)
  prev.seed = get(".Random.seed", envir = .GlobalEnv)
  prev.kind = RNGkind()
  set.seed(seed, kind = reg$RNGkind[1L], normal.kind=reg$RNGkind[2L])

  reset = function() {
    RNGkind(kind = prev.kind[1L], normal.kind = prev.kind[2L])
    assign(".Random.seed", prev.seed, envir=.GlobalEnv)
  }

  return(list(reset = reset))
}

addIntModulo = function(x, y, mod = .Machine$integer.max) {
  as.integer((as.double(x) + as.double(y)) %% mod)
}

isOnSlave = function() {
  getOption("BatchJobs.on.slave", default=FALSE)
}

setOnSlave = function(x) {
  checkArg(x, "logical", len=1L, na.ok=FALSE)
  options(BatchJobs.on.slave=x)
}

getOperatingSystem = function() {
  Sys.info()["sysname"]
}

# simple wrapper for load which returns the contents of file as a list. parts
# may be used to load only specific variables. Always returns a list.
load2 = function(file, parts, stop.if.missing=TRUE, ...) {
  ee = new.env()
  load(file, ee)
  if (!missing(parts)) {
    if (stop.if.missing && !all(parts %in% ls(ee))) {
      stopf("Error: '%s' does not contain objects with names '%s'",
            file, collapse(parts[!(parts %in% ls(ee))]))
    }
    return(mget(parts, ee, ...))
  }
  as.list(ee)
}

# simple wrapper for load which loads an RData file and returns exactly
# one object
loadSingleObject = function(file, name, stop.if.missing=TRUE) {
  ee = new.env()
  load(file, ee)
  if(stop.if.missing && !(name %in% ls(ee))) {
    stopf("Error: '%s' does not contain an object with name '%s'",
          file, name)
  }
  ee[[name]]
}

is.evaluable = function(x) {
  return(is.call(x) || is.expression(x) || is.symbol(x))
}
