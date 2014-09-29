library(BBmisc)
options(BBmisc.ProgressBar.style = "off")
options(BatchJobs.verbose = FALSE)

conf = getConfig()
conf$default.resources$walltime = 120
conf$default.resources$memory = 256
conf$mail.start = conf$mail.done = conf$mail.error = "none"
conf$raise.warnings = FALSE
conf$max.concurrent.jobs = Inf
setConfig(conf = conf)
rm(conf)

getWorkDir = function() {
  work.dir = "unittests-files"
  # if env var is set we do the tests in the current wd,
  # if not, we do everything in a subdir of tempdir()
  # required for debians autopkgtest
  if (isExpensiveExampleOk())
    file.path(getwd(), work.dir)
  else
    file.path(tempdir(), work.dir)
}

getTempDir = function() {
  file.path(getWorkDir(), basename(tempfile("tmp")))
}

makeTestRegistry = function(packages = character(0L), work.dir = getWorkDir(), file.dir = getTempDir(), ...) {
  dir.create(file.dir, recursive = TRUE, showWarning = FALSE)
  makeRegistry(
    id = "unittests",
    seed = 1L,
    packages = packages,
    file.dir = file.dir,
    work.dir = work.dir,
    ...
  )
}

# cleanup stray files
cleanup = function() {
  unittests.dir = getWorkDir()
  if (file.exists(unittests.dir))
    return(unlink(unittests.dir, recursive = TRUE) == 0L)
  return(TRUE)
}
