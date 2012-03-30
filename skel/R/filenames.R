checkDir = function(path, create=FALSE, check.empty=FALSE, check.posix=FALSE) {
  if (create) {
    if (file.exists(path)) {
      if (!file.info(path)$isdir)
        stop("File in place where dir should be created: ", path)
    } else {
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

  if(!identical(check.empty, FALSE) &&
     !all(list.files(path, all.files=TRUE) %in% c(".", ".."))) {
    msg = sprintf("Directory '%s' does not seem to be empty!", path)
    if(check.empty == "stop")
      stop(msg)
    warning(msg)
  }

  if(check.posix) {
    pattern = "^[[:alnum:]/_.-]*$"
    if(! grepl(pattern, makePathAbsolute(path)))
      stopf("Directory '%s' contains illegal characters! Allowed: a-z A-Z 0-9 . - _", makePathAbsolute(path))
  }
}

createShardedDirs = function(reg, ids) {
  if (reg$sharding) {
    paths = file.path(getJobParentDir(reg$file.dir), unique(getShardedSubDir(ids)))
    lapply(paths, checkDir, create=TRUE)
  }
}

getOperatingSystem = function() {
  Sys.info()["sysname"]
}

getCurrentDir = function() {
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
    path = gsub("\\", "/", path, fixed=TRUE)
  } else {
    # if path starts with / we use that as a heuristic that we dont have to change it
    if(substr(path, 1L, 1L) != "/")
      path = normalizePath(path, mustWork=TRUE)
  }
  path
}

getShardedSubDir = function(ids) {
  sprintf("%02i", ids %% 100L)
}

getJobParentDir = function(file.dir) {
  file.path(file.dir, "jobs")
}

getFunDir = function(file.dir) {
  file.path(file.dir, "functions")
}

getJobDir = function(reg, id) {
  p = getJobParentDir(reg$file.dir)
  if (reg$sharding)
    p = file.path(p, getShardedSubDir(id))
  p
}

getFilePath = function(reg, id, suffix, ext) {
  fn = as.character(id)
  if (!is.null(suffix))
    fn = paste(fn, "-", suffix, sep="")
  fn = paste(fn, ext, sep=".")
  file.path(getJobDir(reg, id), fn)
}

getConfFilePath = function(reg)
  file.path(reg$file.dir, "conf.RData")

getRegistryFilePath = function(file.dir)
  file.path(file.dir, "registry.RData")

# getJobFilePath = function(reg, id)
#  getFilePath(reg, id, "job", "RData")

getRScriptFilePath = function(reg, id)
  getFilePath(reg, id, NULL, "R")

getLogFilePath = function(reg, id)
  getFilePath(reg, id, NULL, "out")

getResultFilePath = function(reg, id, part=as.character(NA)) {
  s = if (is.na(part)) "result" else paste("result", part, sep="-")
  getFilePath(reg, id, s, "RData")
}
