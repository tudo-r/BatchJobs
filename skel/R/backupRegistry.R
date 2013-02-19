sweepRegistry = function(reg, sweep = "scripts") {
  checkRegistry(reg)
  syncRegistry(reg)
  if (!length(sweep))
    return (invisible(TRUE))
  sweep = match.arg(sweep, choices = c("scripts", "logs", "resources", "conf"), several.ok = TRUE)
  if (length(dbFindRunning(reg)))
    stop("Can't sweep registry while jobs are running")

  fd = reg$file.dir
  files = list.files(fd, pattern = "^killjobs_failed_ids_*")
  if ("resources" %in% sweep)
    files = c(files, list.files(file.path(fd, "resources"), full.names = TRUE))
  if ("conf" %in% sweep)
    files = c(files, list.files(fd, pattern = "^conf.RData$"), full.names = TRUE)
  if (all(c("logs", "scripts") %in% sweep)) {
    files = c(files, list.files(file.path(fd, jobs), pattern = "^[0-9]+\\.[out|R]$", recursive = TRUE, full.names = TRUE))
  } else {
    if ("logs" %in% sweep)
      files = c(files, list.files(file.path(fd, jobs), pattern = "^[0-9]+\\.out$", recursive = TRUE, full.names = TRUE))
    if ("scripts" %in% sweep)
      files = c(files, list.files(file.path(fd, jobs), pattern = "^[0-9]+\\.R$", recursive = TRUE, full.names = TRUE))
  }

  messagef("Removing %i files ...", length(files))
  file.remove(files)
  return(invisible(TRUE))
}


backupRegistry = function(reg, fn, exclude = c("scripts")) {
  checkRegistry(reg)
  syncRegistry(reg)
  if (length(exclude))
    exclude = match.arg(exclude, choices = c("scripts", "logs", "resources", "conf"), several.ok = TRUE)

  if (missing(fn))
    fn = sprintf("%s_%s.zip", reg$id, Sys.Date())
  else
    checkArg(fn, "character", len = 1L)

  if (length(dbFindRunning(reg)))
    stop("All jobs must be terminated before bundling a registry")

  if (file.exists(fn)) {
    warningf("File '%s' already exists. Updating", fn)
    fn = makePathAbsolute(fn)
  } else {
    # *sigh*
    file.create(fn)
    fn = makePathAbsolute(fn)
    file.remove(fn)
  }

  excluded = "killjobs_failed_ids_*"
  if ("conf" %in% exclude)
    excluded = c(excluded, "conf.RData")
  if ("resources" %in% exclude)
    excluded = c(excluded, file.path("resources", "resources_*.RData"))
  if ("scripts" %in% exclude)
    excluded = c(excluded, ifelse(reg$sharding, file.path("jobs", "*", "*.R"), file.path("jobs", "*.R")))
  if ("logs" %in% exclude)
    excluded = c(excluded, ifelse(reg$sharding, file.path("jobs", "*", "*.out"), file.path("jobs", "*.out")))

  fd = basename(reg$file.dir)
  parent = dirname(reg$file.dir)
  extras = paste("-x", collapse(shQuote(file.path(fd, excluded)), " "))
  inDirectory(parent, zip(fn, files = fd, extras = extras))

  invisible(TRUE)
}
