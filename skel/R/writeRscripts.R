writeRscripts = function(reg, ids, disable.mail, delays, interactive.test) {
  if (!interactive.test) {
    template = paste(
      "Sys.sleep(%%f)",
      "options(BatchJobs.on.slave=TRUE)",
      "library(BatchJobs)",
      "BatchJobs:::doJob(",
      "\treg=loadRegistry('%s'),",
      "\tids=c(%%s),",
      "\tmultiple.result.files=%s,",
      "\tdisable.mail=%s,",
      "\tfirst=%i,",
      "\tlast=%i)",
      "BatchJobs:::setOnSlave(FALSE)",
      sep="\n")

  } else {
    template = paste(
      "ignore = %%f",
      "setOnSlave(TRUE)",
      "doJob(",
      "\treg=loadRegistry('%s'),",
      "\tids=c(%%s),",
      "\tmultiple.result.files=%s,",
      "\tdisable.mail=%s,",
      "\tfirst=%i,",
      "\tlast=%i)",
      "setOnSlave(FALSE)",
      sep="\n")
  }

  fids = vapply(ids, head, integer(1L), 1L) # first job id in chunk
  first = head(fids, 1L)
  last = tail(fids, 1L)
  ids.str = vapply(ids, function(id) collapse(paste(id, "L", sep="")), character(1L)) # ids as collapsed strings

  # print the constant arguments (of length 1) into the template
  template = sprintf(template, reg$file.dir, reg$multiple.result.files, disable.mail, first, last)

  # print delays and ids into template. sprintf will return a string of length length(delays) == length(ids.str) == length(ids)
  # put this together with file names into an mapply on cat.
  mapply(FUN=cat, SIMPLIFY=FALSE, USE.NAMES=FALSE,
         sprintf(template, delays, ids.str),
         file = getRScriptFilePath(fids, reg = reg))
}
