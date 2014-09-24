#' Sanitize a path
#'
#' @description
#' Replaces backward slashes with forward slashes and optionally
#' normalizes the path.
#'
#' @param path [\code{character}]\cr
#'  Vector of paths to sanitize.
#' @param make.absolute [\code{logical}]\cr
#'  If \code{TRUE} convert to an absolute path.
#' @param normalize.absolute [\code{logical}]\cr
#'  Also call \code{\link[base]{normalizePath}} on absolute paths?
#'  This will immediately resolve symlinks.
#' @return \code{character} with sanitized paths.
#' @export
sanitizePath = function(path, make.absolute = TRUE, normalize.absolute = FALSE) {
  assertCharacter(path, any.missing = FALSE)
  assertFlag(make.absolute)
  assertFlag(normalize.absolute)
  if (make.absolute) {
    normalize = if (normalize.absolute) rep.int(TRUE, length(path)) else !isPathFromRoot(path)
    path[normalize] = normalizePath(path[normalize], mustWork = FALSE, winslash = "/")
  }
  gsub("\\", "/", path, fixed = TRUE)
}
