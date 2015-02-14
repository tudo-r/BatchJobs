#' Evaluates expressions as jobs on a registry
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param exprs [\code{\link{expression}}]\cr
#'   R expressions to evaluate.
#' @param ... [any]\cr
#'   Additional arguments passed to \code{\link{batchMap}}.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @seealso This function is analogous to
#' \code{parallel::\link[parallel]{clusterEvalQ}}.
#'
#' @export
batchEvalQ <- function(reg, exprs, ...) {
  fun <- function(expr, ..., envir=globalenv()) {
    eval(substitute(expr), envir=envir)
  }
  batchMap(reg, fun=fun, exprs, ...)
}

