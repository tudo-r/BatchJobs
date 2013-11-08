makeTestRegistry = function(packages=character(0), ...) {
  if(unlink("unittests-files", recursive=TRUE) != 0)
    stop("Could not delete unittests-files!")
  makeRegistry(
    id = "unittests",
    seed = 1,
    packages=packages,
    ...
  )
}

overloaded.index.obj = list(els = 3:1)
class(overloaded.index.obj) = "OverloadedIndex"
  
length.OverloadedIndex = function(x) length(x$els)

`[.OverloadedIndex` = function(x, ..., drop = TRUE) {
  x$els[...]
}

`[[.OverloadedIndex` = function(x, ..., drop = TRUE) {
  x$els[[...]]
}
