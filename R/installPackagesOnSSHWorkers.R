#' Install packages on SSH workers.
#'
#' @description
#' Installation is done via \code{\link{callFunctionOnSSHWorkers}}
#' and \code{\link{install.packages}}.
#'
#' Note that as usual the function tries to install
#' the packages into the first path of \code{.libPaths()}
#' of each each worker.
#'
#' @param nodenames [\code{character}]\cr
#'   Nodenames of workers.
#' @param pkgs [\code{character}]\cr
#'   See \code{\link{install.packages}}.
#' @param repos [\code{character}]\cr
#'   See \code{\link{install.packages}}.
#'   If the user must be queried this is of course done on the master.
#' @param consecutive [\code{logical(1)}]\cr
#'   See \code{\link{callFunctionOnSSHWorkers}}.
#'   Default is \code{TRUE}.
#' @param show.output [\code{logical(1)}]\cr
#'   See \code{\link{callFunctionOnSSHWorkers}}.
#'   Default is \code{consecutive}.
#' @param ... [any]\cr
#'   Passed to \code{\link{install.packages}}.
#' @return Nothing.
#' @export
#' @seealso \code{\link{callFunctionOnSSHWorkers}}
installPackagesOnSSHWorkers = function(nodenames, pkgs,
  repos = getOption("repos"), consecutive = TRUE, show.output = consecutive, ...) {

  assertCharacter(pkgs, min.len = 1L, any.missing = FALSE)
  if (repos == "@CRAN@")
    repos = chooseCRANmirror()
  callFunctionOnSSHWorkers(nodenames, fun = install.packages,
    pkgs = pkgs, repos = repos, consecutive = consecutive, show.output = show.output, ...)
  invisible(NULL)
}
