loadJobFunction = function(reg, job) {
  fun.dir = getFunDir(reg$file.dir)
  fn = file.path(fun.dir, sprintf("%s.RData", job$fun.id))
  job$fun = load2(fn)$fun
  return(job)
}
