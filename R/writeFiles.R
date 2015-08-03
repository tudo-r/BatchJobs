writeFiles = function(reg, cf, ids, chunks.as.arrayjobs, resources.timestamp, disable.mail, staged, delay) {
  ### write r script files
  template = paste(
    "Sys.sleep(%f)",
    "options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '%s')",
    "library(BatchJobs)",
    "res = BatchJobs:::doJob(",
    "\treg = loadRegistry('%s'),",
    "\tids = c(%s),",
    "\tmultiple.result.files = %s,",
    "\tstaged = %s,",
    "\tdisable.mail = %s,",
    "\tfirst = %iL,",
    "\tlast = %iL,",
    "\tarray.id = %s)",
    "BatchJobs:::setOnSlave(FALSE)",
    sep = "\n")

  first = head(ids, 1L)
  last = tail(ids, 1L)

  # print the constant arguments (of length 1) into the template
  resources.path = getResourcesFilePath(reg, resources.timestamp)
  array.str = if (chunks.as.arrayjobs) sprintf("Sys.getenv(\"%s\", NA)", cf$getArrayEnvirName()) else NA
  template = sprintf(template, delay, resources.path, reg$file.dir,
                     collapse(paste0(ids, "L")),
                     reg$multiple.result.files, staged, disable.mail, first, last, array.str)
  r.file = getRScriptFilePath(reg, first)
  cat(template, file = r.file)

  ### if staged is FALSE, also write jobs to file system
  if (staged) {
    job.files = character(0L)
  } else {
    job.files = getJobFile(reg, ids)
    Map(f = saveRDS, object = getJobs(reg, ids), file = job.files)
  }

  invisible(c(r.file, job.files))
}
