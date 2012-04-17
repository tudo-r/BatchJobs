checkIds = function(reg, ids) {
  dup = which(duplicated(ids))
  if (length(dup))
    stopf("You have duplicated entries in your id vector: %s", collapse(ids[dup]))
  ids = setdiff(ids, dbGetJobIds(reg))
  if (length(ids) > 0L)
    stopf("Ids not present in registry: %s", collapse(ids))
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
# may be used to load only specific variables.
# always returns a list.
load2 = function(file, parts, ...) {
  ee = new.env()
  load(file, ee)
  if (!missing(parts))
    return(mget(parts, ee, ...))
  as.list(ee)
}
