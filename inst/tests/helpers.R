options(BBmisc.ProgressBar.style = "off")

makeTestRegistry = function(packages=character(0), ..., cleanup=TRUE) {
  fd = "unittests-files"
  rd = file.path(fd, "registry")
  if (cleanup) {
    if(file.exists(fd) && unlink(fd, recursive=TRUE) != 0)
      stop("Could not delete unittests-files!")
  } else {
    if(file.exists(rd) && unlink(rd, recursive=TRUE) != 0)
      stop("Could not delete registry dir")
  }
  dir.create(fd, recursive=TRUE, showWarning=FALSE)
  makeRegistry(
    id = "unittests",
    seed = 1,
    packages=packages,
    file.dir=rd,
    ...
  )
}

# tempfile
tf = function() file.path("unittests-files", basename(tempfile()))

overloaded.index.obj = list(els = 3:1)
class(overloaded.index.obj) = "OverloadedIndex"

length.OverloadedIndex = function(x) length(x$els)

`[.OverloadedIndex` = function(x, ..., drop = TRUE) {
  x$els[...]
}

`[[.OverloadedIndex` = function(x, ..., drop = TRUE) {
  x$els[[...]]
}
