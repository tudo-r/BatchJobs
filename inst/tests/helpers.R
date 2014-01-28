library(BBmisc)
options(BBmisc.ProgressBar.style = "off")

conf = getConfig()
conf$default.resources$walltime = 120
conf$default.resources$memory = 256
conf$mail.start = conf$mail.done = conf$mail.error = "none"
conf$raise.warnings = FALSE
conf$max.concurrent.jobs = Inf
setConfig(conf = conf)
rm(conf)


cleanup = function() {
  dir = "unittests-files"
  if (!file.exists(dir))
    return(TRUE)

  i = 1L
  repeat {
    if(unlink(dir, recursive=TRUE) == 0L)
      return(TRUE)
    if (i == 6L)
      return(FALSE)

    i = i + 1L
    Sys.sleep(5)
  }
}

tf = function() {
  file.path("unittests-files", basename(tempfile("unittest")))
}

makeTestRegistry = function(packages=character(0L), ...) {
  fd = tf()
  rd = file.path(fd, "registry")
  dir.create(fd, recursive=TRUE, showWarning=FALSE)
  makeRegistry(
    id = "unittests",
    seed = 1,
    packages=packages,
    file.dir=rd,
    work.dir="unittests-files",
    ...
  )
}

in.dir = function(dir, expr) {
  old = setwd(dir)
  on.exit(setwd(old))
  force(expr)
}

stopifnot(cleanup())

# overloaded.index.obj = list(els = 3:1)
# class(overloaded.index.obj) = "OverloadedIndex"
#
# length.OverloadedIndex = function(x) length(x$els)
#
# `[.OverloadedIndex` = function(x, ..., drop = TRUE) {
#   x$els[...]
# }
#
# `[[.OverloadedIndex` = function(x, ..., drop = TRUE) {
#   x$els[[...]]
# }
