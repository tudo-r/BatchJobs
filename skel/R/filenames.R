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

  if (check.empty && !all(list.files(path, all.files=TRUE) %in% c(".", "..")))
    stopf("Directory '%s' does not seem to be empty!", path)

  if(check.posix) {
    pattern = "^[[:alnum:]/_.+-]+$"
    if(! grepl(pattern, makePathAbsolute(path)))
      stopf("Directory '%s' contains illegal characters! Allowed: a-z A-Z 0-9 / + . - _", makePathAbsolute(path))
  }
}

createShardedDirs = function(reg, ids) {
  if (reg$sharding) {
    lapply(getJobDirs(reg, ids, unique=TRUE), checkDir, create=TRUE)
  }
}

getCurrentDir = function() {
  #FIXME why do we need pwd? this seems pretty error prone
  #FIXME remove this FIXME with comment or remove this function
  if(getOperatingSystem() == "Windows")
    getwd()
  else
    # it can be problematic to always resolve symbolic links
    system("pwd", intern=TRUE, wait=TRUE)
}

makePathAbsolute = function(path) {
  if (getOperatingSystem() == "Windows") {
    # as we print file paths to R files later on, we must use the forward slash also on windows.
    # winslash arg is not available in slightly older versions of R
    path = normalizePath(path, mustWork=TRUE)
    gsub("\\", "/", path, fixed=TRUE)
  } else {
    # if path starts with / we use that as a heuristic that we dont have to change it
    if(substr(path, 1L, 1L) != "/")
      path = normalizePath(path, mustWork=TRUE)
  }
  path
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

getConfFilePath = function(reg)
  file.path(reg$file.dir, "conf.RData")

getRegistryFilePath = function(file.dir)
  file.path(file.dir, "registry.RData")

getRScriptFilePath = function(reg, id)
  getFilePaths(reg, id, NULL, "R")

getLogFilePath = function(reg, id)
  getFilePaths(reg, id, NULL, "out")

getResultFilePath = function(reg, id, part=as.character(NA)) {
  s = if (is.na(part)) "result" else paste("result", part, sep="-")
  getFilePaths(reg, id, s, "RData")
}
