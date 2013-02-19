# Bundles your file.dir into a zip file.
#
# Creates a zip archive of your \code{file.dir}.
# You can choose which files will be included or excluded.
# Note that only your file.dir will be archived, not your work.dir!
backupRegistry = function(reg, fn, exclude = c("scripts", "logs", "resouces", "conf")) {
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
