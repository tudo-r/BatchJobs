writeRscripts = function(reg, cf, ids, chunks.as.arrayjobs, resources.timestamp,
                         disable.mail, delays, interactive.test) {
  if (!interactive.test) {
    template = paste(
      "Sys.sleep(%%f)",
      "options(BatchJobs.on.slave=TRUE, BatchJobs.resources.path='%s')",
      "library(BatchJobs)",
      "res = BatchJobs:::doJob(",
      "\treg=loadRegistry('%s'),",
      "\tids=c(%%s),",
      "\tmultiple.result.files=%s,",
      "\tdisable.mail=%s,",
      "\tfirst=%iL,",
      "\tlast=%iL,",
      "\tarray.id=%s)",
      "BatchJobs:::setOnSlave(FALSE)",
      sep="\n")

  } else {
    template = paste(
      "ignore = %%f",
      "setOnSlave(TRUE, resources.path='%s')",
      "res = doJob(",
      "\treg=loadRegistry('%s'),",
      "\tids=c(%%s),",
      "\tmultiple.result.files=%s,",
      "\tdisable.mail=%s,",
      "\tfirst=%iL,",
      "\tlast=%iL,",
      "\tarray.id=%s)",
      "setOnSlave(FALSE)",
      sep="\n")
  }


  fids = vapply(ids, head, integer(1L), 1L) # first job id in chunk
  first = head(fids, 1L)
  last = tail(fids, 1L)
  ids = vapply(ids, function(id) collapse(paste(id, "L", sep="")), character(1L)) # ids as collapsed strings

  # print the constant arguments (of length 1) into the template
  resources.path = getResourcesFilePath(reg, resources.timestamp)
  array.str = if (chunks.as.arrayjobs) sprintf("Sys.getenv(\"%s\", NA)", cf$getArrayEnvirName()) else NA
  template = sprintf(template, resources.path, reg$file.dir, reg$multiple.result.files, disable.mail, first, last, array.str)

  # print delays and ids into template. sprintf will return a string of length length(delays) == length(ids)
  # put this together with file names into an mapply on cat.
  mapply(FUN=cat, SIMPLIFY=FALSE, USE.NAMES=FALSE,
         sprintf(template, delays, ids),
         file = getRScriptFilePath(fids, reg = reg))
}
