saveFunctions = function(reg, jobs) {
  fun.dir = getFunDir(reg$file.dir)
  funs = extractSubList(jobs, "fun", simplify=FALSE)
  fun.ids = extractSubList(jobs, "fun.id")

  lapply(which(!duplicated(fun.ids)), function(i) {
    fn = file.path(fun.dir, sprintf("%s.RData", fun.ids[i]))
    save2(file=fn, fun=funs[[i]])
  })
}
