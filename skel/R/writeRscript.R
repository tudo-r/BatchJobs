writeRscript = function(reg, ids, disable.mail, delays, interactive.test, first, last) {
  if (!interactive.test) {
    template = paste(
      "options(BatchJobs.on.slave=TRUE)",
      "library(BatchJobs)",
      "reg = BatchJobs:::loadRegistry('%s')",
      "ids = c(%s)",
      "mult.files = %s",
      "disable.mail = %s",
      "first = %iL",
      "last = %iL",
      "Sys.sleep(%f)",
      "BatchJobs:::doJob(reg, ids, mult.files, disable.mail, first, last)",
      "BatchJobs:::setOnSlave(FALSE)",
      sep = "\n"
   )
  } else {
    template = paste(
      "setOnSlave(TRUE)",
      "reg = loadRegistry('%s')",
      "ids = c(%s)",
      "mult.files = %s",
      "disable.mail = %s",
      "first = %iL",
      "last = %iL",
      # ignore delay here
      "ignore = %f",
      "doJob(reg, ids, mult.files, disable.mail, first, last)",
      "setOnSlave(FALSE)",
      sep = "\n"
    )
  }

  # for chunks we take the first id of the last chunk as "last" job, as first is stored in chunk
  # results and we store the log file under that name, etc
  if (missing(first))
    first = if(is.list(ids)) head(unlist(head(ids, 1L)), 1L) else head(ids, 1L)
  if (missing(last))
    last = if(is.list(ids)) head(unlist(tail(ids, 1L)), 1L) else tail(ids, 1L)

  mapply(function(id, delay) {
    cat(file = getRScriptFilePath(reg, id[1L]),
        sprintf(template, reg$file.dir, collapse(paste(id, "L", sep = "")),
                reg$multiple.result.files, disable.mail, first, last, delay))
    }, ids, delays)
}

