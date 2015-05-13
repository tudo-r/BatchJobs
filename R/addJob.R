saveFunction = function(reg, fun, more.args) {
  fun = checkUserFunction(fun)
  fun.id = digest(list(fun, more.args))
  save2(file = getFunFilePath(reg, fun.id), fun = fun, more.args = more.args)
  return(fun.id)
}
