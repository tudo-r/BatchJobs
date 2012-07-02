#' @import BBmisc
#' @import utils
#' @import DBI
#' @import RSQLite
#' @importFrom digest digest
#' @importFrom brew brew
#' @importFrom sendmailR sendmail
#' @importFrom plyr rbind.fill

.BatchJobs.conf <- new.env()

.onAttach = function(libname, pkgname) {
  # FIXME: we should move this to onLoad when "is"
  # is not used in Bmisc anymore
  if (!isOnSlave()) {
    assignConfDefaults()  
  }
  # only init the conf if we are not in slave process
  # there we load it anyway
  if (!isOnSlave()) {
    # now load stuff from package and userhome
    readConfs()
    # show it
    lapply(capture.output(showConf()), packageStartupMessage)
  }
}
