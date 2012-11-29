#' Print and return R version and configuration information for SSH workers.
#'
#' @param nodenames [\code{character}]\cr
#'   Nodenames of workers.
#' @return [\code{list}]. Results of function calls, named by nodenames.
#' @export
#' @seealso \code{\link{callFunctionOnSSHWorkers}}
getSSHWorkersInfo = function(nodenames) {
  fun = function() {
    lib.paths = .libPaths()
    list(
      r.home = R.home(),
      session.info = sessionInfo(),
      lib.paths = lib.paths,
      is.lib.path.writeable = (file.access(lib.paths[1], 2) == 0)
    )
  }
  res = callFunctionOnSSHWorkers(nodenames, fun=fun, 
    consecutive=FALSE, show.output=FALSE, use.names=TRUE)
  for (nn in nodenames) {  
    r = res[[nn]]
    messagef("Node: %s", nn)
    messagef(r$session.info$R.version$version.string)
    messagef("Platform: %s", r$session.info$platform)
    messagef("R Home: %s", r$r.home)
    messagef("First lib path: %s", r$lib.paths[[1]])
    messagef("")
  }
  notok = names(Filter(function(r) !r$is.lib.path.writeable, res))
  if (length(notok) > 0)
    warningf("On the following nodes the first lib path is not writeable: %s",
      collapse(notok))
  invisible(res)
}
