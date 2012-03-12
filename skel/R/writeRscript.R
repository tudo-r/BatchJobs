writeRscript = function(fn.rscript, file.dir, ids, mult.files, disable.mail, first, last, interactive.test) {
  if (!interactive.test)
    template = paste(
      "library(BatchJobs)",
      "reg = BatchJobs:::loadRegistry('%s')",
      "ids = c(%s)",
      "mult.files = %s",
      "disable.mail = %s",
      "first = %iL",
      "last = %iL",
      "BatchJobs:::doJob(reg, ids, mult.files, disable.mail, first, last)", 
      sep = "\n"
   )
  else 
    template = paste(
      "reg = loadRegistry('%s')",
      "ids = c(%s)",
      "mult.files = %s",
      "disable.mail = %s",
      "first = %iL",
      "last = %iL",
      "doJob(reg, ids, mult.files, disable.mail, first, last)", 
      sep = "\n"
    )
    rscript = sprintf(template, file.dir, collapse(paste(ids, "L", sep = "")), mult.files, disable.mail, first, last)
    cat(file = fn.rscript, rscript)
}
