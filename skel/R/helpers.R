checkIds = function(reg, ids) {
  if (any(duplicated(ids)))
    stop("You have duplicated entries in your id vector!")
  ids = setdiff(ids, dbGetJobIds(reg))
  if (length(ids) > 0L)
    stop("Id is not present in registry: ", ids[1])
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
      stopf("%s because %s cluster functions do not killing of jobs!", msg, cf$name)
  return(fun)  
}

getRandomSeed = function(n = 1L) {
  as.integer(runif(n, 1, floor(.Machine$integer.max / 2L)))
}

seeder = function(seed) {
  if(!exists(".Random.seed", envir = .GlobalEnv))
     runif(1L)
  prev = get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  list(reset = function() assign(".Random.seed", prev, envir=.GlobalEnv))
}

addIntModulo = function(x, y, mod = .Machine$integer.max) {
  as.integer((as.double(x) + as.double(y)) %% mod)
}
