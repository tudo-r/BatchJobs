writeRscripts = function(reg, cf, ids, chunks.as.arrayjobs, resources.timestamp, disable.mail, delays) {
  template = paste(
    "Sys.sleep(%%f)",
    "options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '%s')",
    "library(checkmate)",
    "library(BatchJobs)",
    "res = BatchJobs:::doJob(",
    "\treg = loadRegistry('%s'),",
    "\tids = c(%%s),",
    "\tmultiple.result.files = %s,",
    "\tdisable.mail = %s,",
    "\tfirst = %iL,",
    "\tlast = %iL,",
    "\tarray.id = %s)",
    "BatchJobs:::setOnSlave(FALSE)",
    sep = "\n")
  fids = viapply(ids, head, 1L) # first job id in chunk
  first = head(fids, 1L)
  last = tail(fids, 1L)
  ids = vcapply(ids, function(id) collapse(paste0(id, "L"))) # ids as collapsed strings

  # print the constant arguments (of length 1) into the template
  resources.path = getResourcesFilePath(reg, resources.timestamp)
  array.str = if (chunks.as.arrayjobs) sprintf("Sys.getenv(\"%s\", NA)", cf$getArrayEnvirName()) else NA
  template = sprintf(template, resources.path, reg$file.dir, reg$multiple.result.files, disable.mail, first, last, array.str)

  # print delays and ids into template. sprintf will return a string of length length(delays) == length(ids)
  # put this together with file names into an mapply on cat.
  files = getRScriptFilePath(fids, reg = reg)
  mapply(FUN = cat, SIMPLIFY = FALSE, USE.NAMES = FALSE,
    sprintf(template, delays, ids), file = files)
  invisible(files)
}
