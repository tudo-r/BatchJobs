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
  funs = extractSubList(jobs, "fun", simplify=FALSE) 
  fun.ids = extractSubList(jobs, "fun.id") 
  notdup = which(!duplicated(fun.ids))
  fun.dir = getFunDir(reg$file.dir)
  sapply(notdup, function(i) {
    fn = file.path(fun.dir, sprintf("%s.RData", fun.ids[i]))
    save2(file=fn, fun=funs[[i]])
  })
}
