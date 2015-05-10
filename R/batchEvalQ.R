#' Evaluates expressions as jobs on a registry
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Empty Registry that will store jobs for the mapping.
#' @param exprs [\code{\link{list}}]\cr
#'   A list of \code{\link{expression}}:s to evaluate.
#' @param local [\code{\link{logical}}[1]]\cr
#'   If TRUE, the expression is evaluated wrapped in a
#'   \code{\link[base]{local}} call, otherwise directly
#'   in the global environment (of the job).
#' @param ... [any]\cr
#'   Additional arguments passed to \code{\link{batchMap}}.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @seealso This function work analogously to
#' \code{parallel::\link[parallel]{clusterEvalQ}} but
#' accepts multiple expressions instead of just one.
#'
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' exprs <- list(
#'   A = substitute({
#'     rnorm(10)
#'   }),
#'   B = substitute({
#'     rnorm(20)
#'   })
#' )
#' batchEvalQ(reg, exprs)
#' print(reg)
#'
#' @export
batchEvalQ <- function(reg, exprs, local=FALSE, ...) {
  assertList(exprs)

  if (local) {
    fun <- function(expr, ..., envir=globalenv()) {
      eval(substitute(local(expr)), envir=envir)
    }
  } else {
    fun <- function(expr, ..., envir=globalenv()) {
      eval(substitute(expr), envir=envir)
    }
  }

  batchMap(reg, fun=fun, exprs, ...)
}
