checkIds = function(reg, ids) {
  ids = convertIntegers(ids)
  checkArg(ids, cl="integer", na.ok=FALSE)
  if (anyDuplicated(ids) > 0L) {
    dup = ids[duplicated(ids)]
    stopf("You have duplicated entries in your id vector: %s", collapse(dup))
  }
  checkIdsPresent(reg, ids)
  return(ids)
}

checkId = function(reg, id) {
  id = convertInteger(id)
  checkArg(id, cl="integer", na.ok=FALSE, len=1L)
  checkIdsPresent(reg, id)
  return(id)
}

checkIdsPresent = function(reg, ids) {
  faulty = setdiff(ids, dbGetJobIds(reg))
  if (length(faulty) > 0L)
    stopf("Ids not present in registry: %s", collapse(faulty))
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

checkPart = function(reg, part) {
  if (reg$multiple.result.files) {
    if (!(
      (is.atomic(part) && length(part) == 1L && is.na(part)) ||
      (is.character(part) && !any(is.na(part)))
    ))
      stop("'part' must be NA or a character vector without NAs!")
  } else {
    if (!is.atomic(part) || length(part) != 1L || !is.na(part))
      stop("'part' must be NA because multiple.result.files is FALSE!")
  }
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
  as.integer(runif(n, 1, .Machine$integer.max / 2L))
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

setOnSlave = function(x, resources.path=as.character(NA)) {
  checkArg(x, "logical", len=1L, na.ok=FALSE)
  checkArg(resources.path, "character", len=1L, na.ok=TRUE)
  options(BatchJobs.on.slave=x)
  options(BatchJobs.resources.path=resources.path)
}

getOperatingSystem = function() {
  Sys.info()["sysname"]
}

# Extract a FIRST match for a pattern from a vector of strings.
# @param x [\code{character}]\cr
#   Vector of strings.
# @param x [\code{character(1)}]\cr
#   Regexp pattern. Just 1.
# @return [\code{character}]. Same length as x.
#   Returns NA if pattern was not found.
strextract = function(x, pattern) {
  if (length(x) == 0)
    return(character(0))
  starts = regexpr(pattern, x)
  lens = attr(starts, "match.length")
  stops = starts + lens - 1
  mapply(function(x, start, stop) {
    if (start == -1)
      as.character(NA)
    else
      substr(x, start, stop)
  }, x, starts, stops, USE.NAMES=FALSE)
}

trim = function(x, ltrim=TRUE, rtrim=TRUE) {
  if (ltrim)
    x = sub("^[[:space:]]+", "", x)
  if (rtrim)
    x = sub("[[:space:]]+$", "", x)
  return(x)
}
