library("BBmisc")
library("BatchJobs")
library("testthat")

doExternalTest = function(dir=getwd(), whitespace=FALSE, n=4, long="false", 
  sleep.master=8, sleep.job=300, resources=list()) {
  
  messagef("############################################################")
  messagef("## BEGIN TEST (whitespace=%i)", whitespace)

  id = "external_test"
  if (whitespace)
    fd = "external test"
  else
    fd = "external_test"  
  fd = file.path(dir, fd)
  ok = unlink(fd, recursive=TRUE)
  if (ok != 0)
    stopf("could not delete file dir: %s", fd)
  reg = makeRegistry(id=id, work.dir=dir, file.dir=fd, sharding=FALSE)
  xs = 50 + seq(1, n)
  f = switch(long,
    false = identity,
    sleep = function(x) {Sys.sleep(sleep.job);x},
    expensive = function(i) if (i<=2) i else f(i-1) + f(i-2))
  batchMap(reg, f, xs)
  submitJobs(reg, resources=resources)
  showStatus(reg)
  if (long == "false") {
    Sys.sleep(sleep.master)
    res = reduceResults(reg, fun=function(aggr,job,res) c(aggr, res))
    expect_equal(res, xs)
  } 
  st = showStatus(reg)
  return(reg)
}

doKillTest = function(dir=getwd(), n=4, long="sleep") {
  messagef("Long = %s", long)
  reg = doExternalTest(dir=dir,whitespace=FALSE, n=n, long=long)
  Sys.sleep(2)
  ids = getJobIds(reg)
  conf = BatchJobs:::getBatchJobsConf()
  cf = conf$cluster.functions
  test.workers = cf$name %in% c("Multicore", "SSH")
  getStatus = function() {
    workers = environment(cf$submitJob)$worker.env$workers
    s = lapply(workers, function(w) {
      z = BatchJobs:::getWorkerStatus.WorkerLinux(w, file.dir=reg$file.dir)
      unlist(z[-1])
    })
    s = do.call(rbind, lapply(s, unlist))
    apply(s, 2, sum)
  }
  if (test.workers) {
    # sleep so processes can update their cpu usage
    if (long == "expensive")
      Sys.sleep(3)
    status = getStatus()
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
  st = showStatus(reg)
  expect_equal(st$submitted, n)
  killJobs(reg, ids)
  if (test.workers) {
    status = getStatus()
    print(status)
    expect_equal(status[["n.jobs"]], 0)
  }
  expect_equal(findMissingResults(reg), ids)
  expect_equal(findOnSystem(reg), integer(0))
  expect_equal(findRunning(reg), integer(0))
  st = showStatus(reg)
  expect_equal(st$submitted, 0)
  expect_equal(st$started, 0)
  expect_equal(st$running, 0)
  expect_equal(st$done, 0)
}
