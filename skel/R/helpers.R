checkIds = function(reg, ids, check.present=TRUE) {
  ids = convertIntegers(ids)
  checkArg(ids, cl="integer", na.ok=FALSE)
  if (anyDuplicated(ids) > 0L) {
    dup = ids[duplicated(ids)]
    stopf("You have duplicated entries in your id vector: %s", collapse(dup))
  }
  if (check.present)
    dbCheckJobIds(reg, ids)
  return(ids)
}

checkId = function(reg, id, check.present=TRUE) {
  id = convertInteger(id)
  checkArg(id, cl="integer", na.ok=FALSE, len=1L)
  if (check.present)
    dbCheckJobIds(reg, id)
  return(id)
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
  if (is.null(fun) && !is.null(msg))
    stopf("%s because %s cluster functions do not support listing of jobs!", msg, cf$name)
  return(fun)
}

getKillJob = function(msg=NULL) {
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  fun = cf$killJob
  if (is.null(fun) && !is.null(msg))
    stopf("%s because %s cluster functions do not support killing of jobs!", msg, cf$name)
  return(fun)
}

getBatchIds = function(reg, msg=NULL) {
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
  set.seed(seed, kind = reg$RNGkind[1L], normal.kind=reg$RNGkind[2L])

  return(list(
    reset = function() {
      RNGkind(kind = prev.kind[1L], normal.kind = prev.kind[2L])
      assign(".Random.seed", prev.seed, envir=.GlobalEnv)
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


now = function() {
  as.integer(Sys.time())
}

# Extract a FIRST match for a pattern from a vector of strings.
# @param x [\code{character}]\cr
#   Vector of strings.
# @param x [\code{character(1)}]\cr
#   Regexp pattern. Just 1.
# @return [\code{character}]. Same length as x.
#   Returns NA if pattern was not found.
strextract = function(x, pattern) {
  if (length(x) == 0L)
    return(character(0L))
  starts = regexpr(pattern, x)
  lens = attr(starts, "match.length")
  stops = starts + lens - 1L
  mapply(function(x, start, stop) {
    if (start == -1L)
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

### FIXME this is going to BBmisc!
names2 = function(x, missing.val="") {
  n = names(x)
  if (is.null(n))
    return(rep.int(missing.val, length(x)))
  replace(n, is.na(n) | n == "", missing.val)
}

list2df = function(li, force.names=FALSE, strings.as.factors = default.stringsAsFactors()) {
  if (length(li) == 0L)
    return(as.data.frame(matrix(nrow = 0L, ncol = 0L)))

  if (force.names) {
    li = lapply(li, function(x) setNames(x, make.names(names2(x), unique=TRUE)))
  }

  cols = unique(unlist(lapply(li, names)))

  if (length(cols) == 0L)
    return(as.data.frame(matrix(nrow = length(li), ncol = 0L)))

  res = namedList(cols)
  for(col in cols) {
    tmp = lapply(li, function(it) it[[col]])
    res[[col]] = simplify2array(replace(tmp, vapply(tmp, is.null, logical(1L)), NA))
  }
  as.data.frame(res, stringsAsFactors = strings.as.factors)
}

shortenString = function(x, len, str.short="...") {
  if (is.na(x))
    return(NA_character_)
  if (nchar(x) > len)
    return(paste(substr(x, 1L, len - nchar(str.short)), str.short, sep=""))
  return(x)
}
