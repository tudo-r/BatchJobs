#' Check job ids.
#'
#' @description
#' Simply checks if probided vector of job ids is valid and
#' throws an error if something is odd.
#'
#' @template arg_reg
#' @param ids [\code{integer}]\cr
#'   Vector of job ids.
#' @param check.present [\code{logical(1)}]\cr
#'   Check if the ids are present in the database? Default is \code{TRUE}.
#' @param len [\code{integer(1)}]\cr
#'   Expected length of id vector. Passed to \code{\link[checkmate]{asInteger}}.
#' @return Invisibly returns the vector of ids, converted to integer.
#' @export
checkIds = function(reg, ids, check.present = TRUE, len = NULL) {
  ids = asInteger(ids, any.missing = FALSE, unique = TRUE, len = len)
  if (check.present)
    dbCheckJobIds(reg, ids)
  invisible(ids)
}
