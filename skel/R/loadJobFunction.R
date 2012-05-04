loadJobFunction = function(reg, job) {
  fun.dir = getFunDir(reg$file.dir)
  fn = file.path(fun.dir, sprintf("%s.RData", job$fun.id))
  job$fun = load2(fn, "fun")

  fn = file.path(fun.dir, sprintf("%s-moreArgs.RData", job$fun.id))
  if(file.exists(fn))
    job$more.args = load2(fn, "more.args")
  return(job)
}
