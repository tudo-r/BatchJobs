#' Install packages on SSH workers.
#'
#' Installation is done via \code{\link{callFunctionOnSSHWorkers}}
#' and \code{\link{install.packages}}. 
#'
#' @param nodenames [\code{character}]\cr
#'   Nodenames of workers.
#' @param pkgs [\code{character}]\cr
#'   See \code{\link{install.packages}}. 
#' @param repos [\code{character}]\cr
#'   See \code{\link{install.packages}}. 
#' @param consecutive [\code{logical(1)}]\cr
#'   Do calls consecutively and always wait till each worker is done.
#'   Default is \code{TRUE}.
#' @param show.output [\code{logical(1)}]\cr
#'   Show output of workers on master during installation.
#'   Can be useful to see what is happening.
#'   Con only be used in consecutive mode.
#'   Default is \code{consecutive}.
#' @param ... [any]\cr
#'   Passed to \code{\link{install.packages}}.
#' @return [\code{list}]. Results of function calls, named by nodenames.
#' @export
#' @seealso \code{\link{callFunctionOnSSHWorkers}}
installPackagesOnSSHWorkers = function(nodenames, pkgs, 
  repos=getOption("repos"), consecutive = TRUE, show.output = consecutive, ...) {
  
  checkArg(pkgs, "character", min.len=1L, na.ok=FALSE)
  if (repos == "@CRAN@")
    repos = chooseCRANmirror()
  callFunctionOnSSHWorkers(nodenames, fun=install.packages, 
    pkgs=pkgs, repos=repos, consecutive=consecutive, show.output=show.output, ...)    
}