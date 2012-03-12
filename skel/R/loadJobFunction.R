loadJobFunction = function(reg, job) {
  fun.dir = getFunDir(reg$file.dir)
  ee = new.env()
  fn = file.path(fun.dir, sprintf("%s.RData", job$fun.id))
  load(file=fn, envir=ee)
  job$fun = ee$fun
  return(job)
}
