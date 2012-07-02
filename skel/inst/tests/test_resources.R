context("resources")

test_that("resources", {  
  # use resources on slave
  reg = makeTestRegistry()
  batchMap(reg, function(i) getResources(), 1:2)
  submitJobs(reg, resources=list(walltime=1, memory=2))
  res = list(walltime=1, memory=2)
  expect_equal(loadResult(reg, 1), res)
  
  if (interactive()) {
    expect_equal(testJob(reg, 1, resources=res), res)
  }
  
  # query on master
  res1 = getJobResources(reg)
  res2 = getJobResources(reg, 1:2)
  expect_equal(res1, res2)
  expect_equal(res1, list('1'=res, '2'=res))
  
  #submit only a few jobs
  resetJobs(reg, ids=1:2, force=TRUE)
  submitJobs(reg, ids=1, resources=list(walltime=1, memory=2))
  expect_equal(loadResult(reg, 1), res)
  expect_error(getJobResources(reg, 1:2), "not been submitted")
  res1 = getJobResources(reg)
  res2 = getJobResources(reg, 1)
  expect_equal(res1, res2)
  expect_equal(res1, list('1'=res))
  
  # defaults in conf
  if (interactive()) 
    conf = getBatchJobsConf()
  else
    conf = BatchJobs:::getBatchJobsConf()
  conf$default.resources = list(walltime=1, memory=2, xxx=3)
  reg = makeTestRegistry()
  batchMap(reg, function(i) getResources(), 1)
  submitJobs(reg, resources=list(memory=20))
  res = list(walltime=1, memory=20, xxx=3)
  expect_equal(loadResult(reg, 1), res)
  expect_equal(getJobResources(reg)[[1]], res)
  conf$default.resources = list()
})


