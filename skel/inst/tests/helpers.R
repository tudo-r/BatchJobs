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
