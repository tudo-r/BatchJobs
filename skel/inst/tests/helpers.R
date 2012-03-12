makeTestRegistry = function(packages=character(0)) {
  if(unlink("runit_files", recursive=TRUE) != 0)
    stop("Could not delete runit_files!")
  makeRegistry(
    seed = 1,
    file.dir = "runit_files",
    packages=packages
  )  
}