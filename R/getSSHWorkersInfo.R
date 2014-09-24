#' Print and return R installation and other information for SSH workers.
#'
#' @description
#' Workers are queried in parallel via \code{\link{callFunctionOnSSHWorkers}}.
#'
#' The function will display a warning if the first lib path on the worker
#' is not writable as this indicates potential problems in the configuration
#' and \code{\link{installPackagesOnSSHWorkers}} will not work.
#'
#' @param nodenames [\code{character}]\cr
#'   Nodenames of workers.
#' @return [\code{list}]. Displayed information as a list named by nodenames.
#' @export
#' @seealso \code{\link{callFunctionOnSSHWorkers}}
getSSHWorkersInfo = function(nodenames) {
  fun = function() {
    lib.paths = .libPaths()
    list(
      r.home = R.home(),
      session.info = sessionInfo(),
      lib.paths = lib.paths,
      is.lib.path.writeable = is.accessible(head(lib.paths, 1L))
    )
  }
  res = callFunctionOnSSHWorkers(nodenames, fun = fun,
    consecutive = FALSE, show.output = FALSE, use.names = TRUE, simplify = FALSE)
  for (nn in nodenames) {
    r = res[[nn]]
    messagef("Node: %s", nn)
    messagef(r$session.info$R.version$version.string)
    messagef("Platform: %s", r$session.info$platform)
    messagef("R Home: %s", r$r.home)
    messagef("First lib path: %s", head(r$lib.paths, 1L))
    messagef("")
  }
  notok = names(Filter(function(r) !r$is.lib.path.writeable, res))
  if (length(notok) > 0L)
    warningf("On the following nodes the first lib path is not writeable: %s",
      collapse(notok))
  invisible(res)
}
