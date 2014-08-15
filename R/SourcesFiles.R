#' @title Add source files to registry.
#'
#' @description
#' Mutator function for \code{src.files} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param src.files [\code{character}]\cr
#'   Paths to add to registry.
#'   See \code{\link{makeRegistry}}.
#' @param src.now [\code{logical(1)}]
#'   Source files now on master?
#'   Default is \code{TRUE}.
#' @template ret_reg_mut
#' @family exports
#' @export
addRegistrySourceFiles = function(reg, src.files, src.now = TRUE) {
  checkRegistry(reg)
  assertCharacter(src.files, any.missing = FALSE)
  if (src.now)
    sourceRegistryFilesInternal(reg$work.dir, character(0L), src.files)
  reg$src.files = c(reg$src.files, src.files)
  saveRegistry(reg)
  return(reg)
}

#' @title Add source dirs to registry.
#'
#' @description
#' Mutator function for \code{src.dirs} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param src.dirs [\code{character}]\cr
#'   Paths to add to registry.
#'   See \code{\link{makeRegistry}}.
#' @param src.now [\code{logical(1)}]
#'   Source files now on master?
#'   Default is \code{TRUE}.
#' @template ret_reg_mut
#' @family exports
#' @export
addRegistrySourceDirs = function(reg, src.dirs, src.now = TRUE) {
  checkRegistry(reg)
  assertCharacter(src.dirs, any.missing = FALSE)
  if (src.now)
    sourceRegistryFilesInternal(reg$work.dir, src.dirs, character(0L))
  reg$src.dirs = c(reg$src.dirs, src.dirs)
  saveRegistry(reg)
  return(reg)
}


#' @title Remove source files from registry.
#'
#' @description
#' Mutator function for \code{src.files} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param src.files [\code{character}]\cr
#'   Paths to remove from registry.
#' @template ret_reg_mut
#' @family exports
#' @export
removeRegistrySourceFiles = function(reg, src.files) {
  checkRegistry(reg)
  assertCharacter(src.files, any.missing = FALSE)
  reg$src.files = setdiff(reg$src.files, src.files)
  saveRegistry(reg)
  return(reg)
}

#' @title Remove packages from registry.
#'
#' @description
#' Mutator function for \code{src.dirs} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param src.dirs [\code{character}]\cr
#'   Paths to remove from registry.
#' @template ret_reg_mut
#' @family exports
#' @export
removeRegistrySourceDirs = function(reg, src.dirs) {
  checkRegistry(reg)
  assertCharacter(src.dirs, any.missing = FALSE)
  reg$src.dirs = setdiff(reg$src.dirs, src.dirs)
  saveRegistry(reg)
  return(reg)
}


