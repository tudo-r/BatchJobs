#' Get the physical location of job files on the hard disk.
#' @template arg_reg
#' @template arg_ids
#' @export
#' @return [\code{character}] Vector of directories.
getJobLocation = function(reg, ids) {
  checkRegistry(reg)
  getJobDirs(reg, ids)
}
