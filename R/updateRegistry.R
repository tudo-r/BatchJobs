#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [any]. Updated \code{\link{Registry}} or \code{FALSE} if no updates were performed.
#' @keywords internal
#' @export
updateRegistry = function(reg) {
    UseMethod("updateRegistry")
}


#' @method updateRegistry Registry
#' @export
updateRegistry.Registry = function(reg) {
  # Fix for missing package version (package versions < 1.0.527)
  if ("BatchJobs" %nin% names(reg$packages)) {
    reg$packages$BatchJobs = list(version = package_version("1.0.527"))
  }

  version.reg = reg$packages$BatchJobs$version
  version.pkg = packageVersion("BatchJobs")

  if (version.reg == version.pkg) {
    return(FALSE)
  }
  if (version.reg > version.pkg) {
    warningf("The registry has been used with BatchJobs version %s, installed is version %s. You should update BatchJobs on this machine.",
             version.reg, version.pkg)
    return(FALSE)
  }

  # update registry
  info("Updating Registry and DB to newer version.")
  if (version.reg < package_version("1.0.606")) {
    # create new resources dir
    resources.dir = getResourcesDir(reg$file.dir)
    checkDir(resources.dir, create = TRUE, check.empty = TRUE)
    query = sprintf("ALTER TABLE %s_job_status ADD COLUMN resources_timestamp INTEGER", reg$id)
    dbDoQuery(reg, query, flags = "rwc")

    # save dummy resources
    query = sprintf("UPDATE %s_job_status SET resources_timestamp = 0 WHERE submitted IS NOT NULL", reg$id)
    dbDoQuery(reg, query, flags = "rwc")
    saveResources(reg, resources = list(), timestamp = 0L)
  }

  if (version.reg < package_version("1.0.723")) {
    checkDir(getPendingDir(reg$file.dir), create = TRUE)
  }

  if (version.reg < package_version("1.0.1071")) {
    checkDir(getExportDir(reg$file.dir), create = TRUE)
  }

  if (version.reg < package_version("1.1")) {
    query = sprintf("ALTER TABLE %s_job_def ADD COLUMN jobname TEXT", reg$id)
    dbDoQuery(reg, query, flags = "rwc")
    reg$src.dirs = character(0L)
  }

  if (version.reg < package_version("1.2")) {
    reg$src.files = character(0L)
  }

  if (version.reg < package_version("1.4")) {
    query = sprintf("ALTER TABLE %s_job_status ADD COLUMN memory REAL", reg$id)
    dbDoQuery(reg, query, flags = "rwc")
  }

  reg$packages$BatchJobs$version = version.pkg
  reg
}

adjustRegistryPaths = function(reg, file.dir, work.dir) {
  adjusted = FALSE

  # adjust file dir if necessary
  file.dir = sanitizePath(file.dir, make.absolute = TRUE)
  if (!isDirectory(file.dir))
    stopf("file.dir does not exist or is not a directory: %s", file.dir)
  if (reg$file.dir != file.dir) {
    reg$file.dir = file.dir
    adjusted = TRUE
  }

  # adjust work dir if necessary
  if (missing(work.dir)) {
    if (!isDirectory(reg$work.dir))
      warningf("The currently set work.dir '%s' does not exists. Use option 'work.dir' in loadRegistry to change it.", reg$work.dir)
  } else {
    work.dir = sanitizePath(work.dir, make.absolute = TRUE)
    if (!isDirectory(work.dir))
      stopf("work.dir does not exist or is not a directory: %s", work.dir)
    reg$work.dir = work.dir
    adjusted = TRUE
  }

  if (adjusted) reg else FALSE
}
