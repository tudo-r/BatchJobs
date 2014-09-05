checkDir = function(path, create = FALSE, check.empty = FALSE, check.posix = FALSE, msg = FALSE) {
  if (create) {
    if (file.exists(path)) {
      if (!isDirectory(path))
        stop("File in place where dir should be created: ", path)
    } else {
      if (msg)
        info("Creating dir: %s", path)
      if (!dir.create(path))
        stopf("Could not create dir: %s", path)
    }
  }

  if (!isDirectory(path))
    stopf("Directory '%s' does not exists", path)

  if (!is.accessible(path))
    stopf("Directory '%s' is not readable/writable!", path)

  if (check.empty && any(list.files(path, all.files = TRUE) %nin% c(".", "..")))
    stopf("Directory '%s' does not seem to be empty!", path)

  if (check.posix && getOption("BatchJobs.check.posix", TRUE)) {
    path.abs = sanitizePath(path, make.absolute = TRUE)
    if(! grepl("^[[:alnum:]:/_.-]+$", path.abs))
      stopf("Directory '%s' contains characters that are not fully portable according to POSIX standards. Allowed: a-z A-Z 0-9 : / . - _", path.abs)
  }
}

checkDirs = function(paths, ...) {
  vcapply(paths, checkDir)
}

createShardedDirs = function(reg, ids) {
  if (reg$sharding) {
    lapply(getJobDirs(reg, ids, unique = TRUE), checkDir, create = TRUE)
  }
}

# tests a directory for read and write permissions
# uses a heuristic for windows
is.accessible = function(path) {
  if (isWindows()) {
    # Workaround: No POSIX file system informations available, use a heuristic
    rnd = basename(tempfile(""))
    tf1 = file.path(path, sprintf("test_write_access_file_%s", rnd))
    td1 = file.path(path, sprintf("test_write_access_dir_%s", rnd))
    tf2 = file.path(td1, "test_write_access_subfile")
    td2 = file.path(td1, "test_write_access_subdir")

    # on exit, try to clean up the mess we might have caused
    on.exit(try(unlink(c(td1, td2, tf1, tf2), recursive = TRUE)))

    # perform the checks
    ok = try({
      file.create(tf1) && identical(readLines(tf1), character(0L)) && file.remove(tf1) &&
      dir.create(td1) && dir.create(td2) && length(list.files(td1)) == 1L &&
      file.create(tf2) && identical(readLines(tf2), character(0L)) && file.remove(tf2) &&
      unlink(td1, recursive = TRUE) == 0L
    })

    if (is.error(ok) || !isTRUE(ok))
      return(FALSE)

    # we don't need the on exit handler anymore, everything should be fine
    on.exit(NULL)
    return(TRUE)
  }

  return(file.access(path, mode = c(2L, 4L)) == 0L)
}

isPathFromRoot = function(path) {
  (isWindows() & grepl("^[[:alpha:]]:", path)) | grepl("^[/\\]", path)
}

getJobDirs = function(reg, ids, unique = FALSE) {
  if (reg$sharding) {
    shards = sprintf("%02i", ids %% 100L)
    if(unique)
      shards = unique(shards)
    return(file.path(reg$file.dir, "jobs", shards))
  }
  file.path(reg$file.dir, "jobs")
}

getFilePaths = function(reg, id, suffix, ext) {
  fn = sprintf("%i%s.%s", id, ifelse(is.null(suffix), "", paste0("-", suffix)), ext)
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

getResultFilePath = function(reg, id, part = NA_character_) {
  s = if (is.na(part)) "result" else paste0("result-", part)
  getFilePaths(reg, id, s, "RData")
}

getResourcesDir = function(file.dir)
  file.path(file.dir, "resources")

getResourcesFilePath = function(reg, timestamp)
  file.path(getResourcesDir(reg$file.dir), sprintf("resources_%i.RData", timestamp))

getPendingDir = function(file.dir)
  file.path(file.dir, "pending")

getExportDir = function(file.dir)
  file.path(file.dir, "exports")

getPendingFile = function(reg, type, id, char = .OrderChars[type]) {
  file.path(getPendingDir(reg$file.dir), sprintf("pending_%s_%s_%i.sql", char, type, id))
}
