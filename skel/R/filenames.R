checkDir = function(path, create=FALSE, check.empty=FALSE, check.posix=FALSE, msg=FALSE) {
  if (create) {
    if (file.exists(path)) {
      if (!file.info(path)$isdir)
        stop("File in place where dir should be created: ", path)
    } else {
      if (msg)
        message("Creating dir: ", path)
      if (!dir.create(path))
        stop("Could not create dir: ", path)
    }
  }

  if (! file.exists(path)) {
    stopf("Directory '%s' does not exists", path)
  }

  if (file.access(path, mode=2L) != 0L)
    stopf("Directory '%s' is not writable!", path)

  if (check.empty && any(list.files(path, all.files=TRUE) %nin% c(".", "..")))
    stopf("Directory '%s' does not seem to be empty!", path)

  if(check.posix) {
    path.abs = makePathAbsolute(path)
    if(! grepl("^[[:alnum:]:/_.+-]+$", path.abs))
      stopf("Directory '%s' contains illegal characters! Allowed: a-z A-Z 0-9 : / + . - _", path.abs)
  }
}

createShardedDirs = function(reg, ids) {
  if (reg$sharding) {
    lapply(getJobDirs(reg, ids, unique=TRUE), checkDir, create=TRUE)
  }
}

makePathAbsolute = function(path) {
  if(substr(path, 1L, 1L) != "/")
    path = normalizePath(path, mustWork=FALSE)
  # TODO:
  # emulate winslash-behaviour
  # remove this in a future version
  if (grepl("windows", tolower(getOperatingSystem()), fixed=TRUE))
    path = gsub("\\", "/", path, fixed=TRUE)

  return(path)
}

getJobDirs = function(reg, ids, unique=FALSE) {
  if (reg$sharding) {
    shards = sprintf("%02i", ids %% 100L)
    if(unique)
      shards = unique(shards)
    return(file.path(reg$file.dir, "jobs", shards))
  }
  file.path(reg$file.dir, "jobs")
}

getFilePaths = function(reg, id, suffix, ext) {
  fn = sprintf("%i%s.%s", id, ifelse(is.null(suffix), "", paste("-", suffix, sep="")), ext)
  file.path(getJobDirs(reg, id), fn)
}

getJobParentDir = function(file.dir)
  file.path(file.dir, "jobs")

getFunDir = function(file.dir)
  file.path(file.dir, "functions")

getFunFilePath = function(reg, fun.id)
  file.path(getFunDir(reg$file.dir), sprintf("%s.RData", fun.id))

getConfFilePath = function(reg)
  file.path(reg$file.dir, "conf.RData")

getRegistryFilePath = function(file.dir)
  file.path(file.dir, "registry.RData")

getRScriptFilePath = function(reg, id)
  getFilePaths(reg, id, NULL, "R")

getLogFilePath = function(reg, id)
  getFilePaths(reg, id, NULL, "out")

getResultFilePath = function(reg, id, part=NA_character_) {
  s = if (is.na(part)) "result" else paste("result", part, sep="-")
  getFilePaths(reg, id, s, "RData")
}
