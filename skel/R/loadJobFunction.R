loadJobFunction = function(reg, job) {
  fun.dir = getFunDir(reg$file.dir)
  fn = file.path(fun.dir, sprintf("%s.RData", job$fun.id))
  job$fun = loadSingleObject(fn, "fun")

  fn = file.path(fun.dir, sprintf("%s-moreArgs.RData", job$fun.id))
  if(file.exists(fn))
    job$more.args = loadSingleObject(fn, "more.args")
  return(job)
}
