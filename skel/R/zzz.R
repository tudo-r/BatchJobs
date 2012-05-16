#' @import BBmisc

.BatchJobs.conf <- new.env()


.onLoad = function(libname, pkgname) {
  if (!isOnSlave()) {
    assignConfDefaults()  
  }
}

.onAttach = function(libname, pkgname) {
  # only init the conf if we are not in slave process
  # there we load it anyway
  if (!isOnSlave()) {
    # now load stuff from package and userhome
    readConfs()
    # show it
    lapply(capture.output(showConf()), packageStartupMessage)
  }
}
