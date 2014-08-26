#' Sanitize a path
#'
#' Replaces backward slashes with forward slashes and optionally
#' normalizes the path.
#'
#' @param path [\code{character}]\cr
#'  Vector of paths to sanitize.
#' @param normalize [\code{logical}]\cr
#'  Do call \code{\link[base]{normalizePath}} on \code{path}.
#'  Default is \code{NULL} which will only normalize if the path
#'  is already an absolute path, i.e. relative paths will remain unchanged.
#' @return \code{character} with sanitized paths.
#' @export
sanitizePath = function(path, normalize = NULL) {
  assertCharacter(path, any.missing = FALSE)
  if (is.null(normalize)) {
    normalize = isPathFromRoot(path)
  } else {
    assert(checkFlag(normalize), checkLogical(normalize, len = length(path)))
    if (length(normalize) == 1L)
      normalize = rep.int(normalize, length(path))
  }
  path[normalize] = normalizePath(path[normalize], mustWork = FALSE, winslash = "/")
  gsub("\\", "/", path, fixed = TRUE)
}
