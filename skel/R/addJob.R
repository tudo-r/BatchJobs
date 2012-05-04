# adds job to registry
addJobs = function(reg, jobs, ...) {
  messagef("Adding %i jobs to DB.", length(jobs))
  saveFunctions(reg, jobs)
  add.res = dbAddJobs(reg, jobs, ...)

  # we can only create the dir after we have obtained the ids fromn the DB
  createShardedDirs(reg, add.res$job.ids)
  return(add.res)
}


addJob = function(reg, job) {
  addJobs(reg, list(job))
}


saveFunctions = function(reg, jobs) {
  fun.dir = getFunDir(reg$file.dir)
  funs = extractSubList(jobs, "fun", simplify=FALSE)
  fun.ids = extractSubList(jobs, "fun.id")
  inds = which(!duplicated(fun.ids))
  Map(saveFunction, funs[inds], fun.ids[inds], reg=reg, fun.dir=fun.dir)
}

saveFunction = function(reg, fun, fun.id, fun.dir) {
  environment(fun) = emptyenv()
  if (missing(fun.id))
    fun.id = digest(fun)
  if (missing(fun.dir))
    fun.dir = getFunDir(reg$file.dir)
  fn = file.path(fun.dir, sprintf("%s.RData", fun.id))
  save2(file=fn, fun=fun)
  return(fun.id)
}
