#' @import BBmisc

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  # set reasonable defaults just to make sure
  assignConfDefaults()  
  # now load stuff from package and userhome
  useDefaultConfs()
  # show it
  showConf()
}
