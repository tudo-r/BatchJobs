#' @import BBmisc

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  # only init the conf if we are not in slave process
  # there we load it anyway
  if (!isOnSlave()) {
    # set reasonable defaults just to make sure
    assignConfDefaults()  
    # now load stuff from package and userhome
    useDefaultConfs()
    # show it
    lapply(capture.output(showConf()), packageStartupMessage)
  }
}
