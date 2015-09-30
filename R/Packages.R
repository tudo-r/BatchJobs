#' @title Add packages to registry.
#'
#' @description
#' Mutator function for \code{packages} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param packages [\code{character}]\cr
#'   Packages to add to registry.
#' @template ret_reg_mut
#' @family exports
#' @export
addRegistryPackages = function(reg, packages) {
  checkRegistry(reg, writeable = TRUE)
  assertCharacter(packages, any.missing = FALSE)
  packages = setdiff(packages, names(reg$packages))
  packages = setNames(lapply(packages, function(pkg) list(version = packageVersion(pkg))), packages)
  reg$packages = c(reg$packages, packages)
  saveRegistry(reg)
}

#' @title Remove packages from registry.
#'
#' @description
#' Mutator function for \code{packages} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param packages [\code{character}]\cr
#'   Packages to remove from registry.
#' @template ret_reg_mut
#' @family exports
#' @export
removeRegistryPackages = function(reg, packages) {
  checkRegistry(reg, writeable = TRUE)
  assertCharacter(packages, any.missing = FALSE)
  mandatory = names(filterNull(extractSubList(reg$packages, "mandatory")))
  reg$packages = reg$packages[names(reg$packages) %nin% setdiff(packages, mandatory)]
  saveRegistry(reg)
}

#' @title Set packages for a registry.
#'
#' @description
#' Mutator function for \code{packages} in \code{\link{makeRegistry}}.
#'
#' @template arg_reg
#' @param packages [\code{character}]\cr
#'   Character vector of package names to load.
#' @template ret_reg_mut
#' @family exports
#' @export
setRegistryPackages = function(reg, packages) {
  checkRegistry(reg, writeable = TRUE)
  assertCharacter(packages, any.missing = FALSE)
  mandatory = names(filterNull(extractSubList(reg$packages, "mandatory")))
  packages = unique(c(mandatory, packages))
  reg$packages = setNames(lapply(packages, function(pkg) list(version = packageVersion(pkg))), packages)
  saveRegistry(reg)
}
