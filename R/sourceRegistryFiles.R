#' Source registry files
#'
#' @description
#' Sources all files found in \code{src.dirs} and specified via \code{src.files}.
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param envir [\code{environment}]\cr
#'   Environment to source the files into. Default is the global environment.
#' @return Nothing.
#' @export
sourceRegistryFiles = function(reg, envir = .GlobalEnv) {
  checkRegistry(reg)
  assertEnvironment(envir)
  sourceRegistryFilesInternal(reg$work.dir, reg$src.dirs, reg$src.files)
}

sourceRegistryFilesInternal = function(work.dir, dirs, files, envir = .GlobalEnv) {
  # add work dir if not /foo/bar path
  w = !isPathFromRoot(files)
  files[w] = file.path(work.dir, files[w])
  w = which.first(!file.exists(files))
  if (length(w))
    stopf("Files to source not found, e.g. %s", files[w])

  w = !isPathFromRoot(dirs) & !grepl("^~", dirs)
  dirs[w] = file.path(work.dir, dirs[w])
  w = which.first(!isDirectory(dirs))
  if (length(w))
    stopf("Directories to source not found, e.g. %s", dirs[w])

  # detect source package directories
  if( length(dirs) ){
      wpkg <- which(sapply(file.path(dirs, 'DESCRIPTION'), file.exists))
      if( length(wpkg) ){
        # setup temporary directory to hold package loading script
        tmpdir <- tempfile('BatchJobs_load_all_')
        dir.create(tmpdir)
        on.exit( unlink(tmpdir, recursive = TRUE) )
        # generate loading scripts
        lapply(dirs[wpkg], function(d){
            cat(sprintf("# load source package\ndevtools::load_all('%s', reset = TRUE)\n", d)
                , file = tempfile(paste0(basename(d), '_'), tmpdir, fileext = ".R"))
        })
        # remove directory from list and add temporary load_all directory
        dirs <- c(dirs[-wpkg], tmpdir)
      }
  } 
  ##
  
  lapply(c(getRScripts(dirs), files), sys.source, envir = envir)
  invisible(TRUE)
}

getRScripts = function(dirs) {
  if (length(dirs)) {
    ok = isDirectory(dirs)
    if (any(!ok))
      stopf("Directories not found: %s", collapse(dirs[!ok]))
    unlist(lapply(dirs, list.files, pattern = "\\.[Rr]$", full.names = TRUE))
  } else {
    character(0L)
  }
}

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
  assertFlag(src.now)
  src.files = sanitizePath(src.files, make.absolute = FALSE)
  if (src.now)
    sourceRegistryFilesInternal(reg$work.dir, character(0L), src.files)
  reg$src.files = union(reg$src.files, src.files)
  saveRegistry(reg)
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
  src.dirs = sanitizePath(src.dirs, make.absolute = FALSE)
  if (src.now)
    sourceRegistryFilesInternal(reg$work.dir, src.dirs, character(0L))
  reg$src.dirs = c(reg$src.dirs, src.dirs)
  saveRegistry(reg)
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
}
