library(testthat)

doExternalTest = function(dir=getwd(), whitespace=FALSE, n=4, long="false") {
  id = "external_test"
  if (whitespace)
    fd = "foo bär"
  else
    fd = "foo"  
  fd = file.path(dir, fd)
  ok = unlink(fd, recursive=TRUE)
  if (ok != 0)
    stopf("could not delete file dir: %s", fd)
  reg = makeRegistry(id=id, work.dir=dir, file.dir=fd, sharding=FALSE)
  xs = 50 + seq(1, n)
  f = switch(long,
    false = identity,
    sleep = function(x) {Sys.sleep(300);x},
    expensive = function(i) if (i<=2) i else f(i-1) + f(i-2))
  batchMap(reg, f, xs)
  submitJobs(reg)
  if (long == "false") {
    Sys.sleep(8)
    res = reduceResults(reg, fun=function(aggr,job,res) c(aggr, res))
    expect_equal(res, xs)
  } 
  return(reg)
}

doKillTest = function(dir=getwd(), test.worker=FALSE, n=4, long="sleep") {
  reg = doExternalTest(dir=dir,whitespace=FALSE, n=n, long=long)
  ids = getJobIds(reg)
  n = length(ids)
  if (test.worker) {
    conf = BatchJobs:::getBatchJobsConf()
    cf = conf$cluster.functions
    stopifnot(cf$name == "Multicore")
    w = environment(cf$submitJob)$workers[[1]]
    # sleep so processes can update their cpu usage
    if (long == "expensive")
      Sys.sleep(3)
    status = BatchJobs:::getWorkerStatus.WorkerLinux(w, reg$file.dir)
    print(status)
    if (long == "sleep") {
      expect_true(status[["n.rprocs"]] >= n)
      expect_true(status[["n.jobs"]] >= n)
    } else if (long == "expensive") {
      expect_true(status[["n.rprocs"]] >= n)
      expect_true(status[["n.rprocs.50"]] >= n)
      expect_true(status[["n.jobs"]] >= n)
    }
  }
  killJobs(reg, ids)
  expect_equal(findMissingResults(reg), ids)
  expect_equal(findOnSystem(reg), integer(0))
  expect_equal(findRunning(reg), integer(0))
}
