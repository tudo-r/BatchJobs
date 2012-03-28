writeRscript = function(fn.rscript, file.dir, ids, mult.files, disable.mail, first, last, interactive.test) {
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
      "doJob(reg, ids, mult.files, disable.mail, first, last)", 
      "setOnSlave(FALSE)",
      sep = "\n"
    )
  }
  rscript = sprintf(template, file.dir, collapse(paste(ids, "L", sep = "")), mult.files, disable.mail, first, last)
  cat(file = fn.rscript, rscript)
}
