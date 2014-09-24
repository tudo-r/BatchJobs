saveFunction = function(reg, fun, more.args) {
  fun = match.fun(fun)
  if (getOption("BatchJobs.clear.function.env")) {
    environment(fun) = .GlobalEnv
  } else {
    ee = environment(fun)
    if (!is.null(ee) && !isNamespace(ee)) {
      nn = ls(ee, all.names = TRUE)
      if (sum(vnapply(nn, function(nn) object.size(ee[[nn]])) / 1024^2) > 10)
        warning("The environment of provided function exceeds 10Mb.")
    }
  }
  fun.id = digest(list(fun, more.args))
  save2(file = getFunFilePath(reg, fun.id), fun = fun, more.args = more.args)
  return(fun.id)
}
