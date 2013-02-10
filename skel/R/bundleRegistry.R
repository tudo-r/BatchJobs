bundleRegistry = function(reg, fn, include = c("logs", "work.dir")) {
  checkRegistry(reg)
  syncRegistry(reg)
  include = match.arg(include, choices = c("scripts", "logs", "resources", "conf", "work.dir"), several.ok = TRUE)
  if (length(dbFindRunning(reg)))
    stop("All jobs must be terminated before bundling a registry")

  if (missing(fn))
    fn = sprintf("%s_%s.zip", reg$id, Sys.Date())
  else
    checkArg(fn, "character", len = 1L)

  # create to normalize, then remove again *sigh*
  file.create(fn)
  fn = makePathAbsolute(fn)
  file.remove(fn)

  # bundle file.dir
  # TODO adjust for win32 and macos
  exclude = character(0L)
  if ("scripts" %nin% include)
    exclude = c(exclude, ifelse(reg$sharding, "jobs/*/*.R", "jobs/*.R"))
  if ("logs" %nin% include)
    exclude = c(exclude, ifelse(reg$sharding, "jobs/*/*.out", "jobs/*.out"))
  if ("resources" %nin% include)
    exclude = c(exclude, "resources/resources_*.RData")
  if ("conf" %nin% include)
    exclude = c(exclude, "conf.RData")

  dir = basename(reg$file.dir)
  parent = dirname(reg$file.dir)
  extras = sprintf("-x '%s'", collapse(file.path(dir, exclude), "' '"))
  inDirectory(parent, zip(fn, files = dir, extras = extras))

  # bundle work.dir
  if ("work.dir" %in% include) {
    dir = basename(reg$work.dir)
    parent = dirname(reg$work.dir)

    # check if file.dir is a subdirectory of work.dir, and exclude it
    rel = subPath(reg$file.dir, reg$work.dir)
    if (!is.na(rel))
      extras = sprintf("-x '%s/*'", file.path(dir, rel))
    else
      extras = ""

    inDirectory(parent, zip(fn, files = dir, extras = extras))
  }

  invisible(TRUE)
}
