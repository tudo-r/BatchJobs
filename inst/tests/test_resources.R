context("resources")

test_that("resources", {
  # FIXME this is still broken for all major cluster system
  # because the resources are stupid!
  # use resources on slave
  reg = makeTestRegistry()
  batchMap(reg, function(i) getResources(), 1:2)
  res = list(walltime=60, memory=100)
  submitJobs(reg, resources=res)
  waitForJobs(reg)
  expect_equal(loadResult(reg, 1)[names(res)], res)

  if (interactive()) {
    expect_equal(testJob(reg, 1, resources=res)[names(res)], res)
  }

  # query on master
  res1 = getJobResources(reg, 1)[[1]]
  res2 = getJobResources(reg, 2)[[1]]
  expect_equal(res1, res2)
  expect_equal(res1[names(res)], res)

  #submit only a few jobs
  resetJobs(reg, ids=1:2, force=TRUE)
  submitJobs(reg, ids=1, resources=res)
  waitForJobs(reg)
  expect_equal(loadResult(reg, 1)[names(res)], res)
  expect_error(getJobResources(reg, 1:2), "not been submitted")
  res1 = getJobResources(reg)
  res2 = getJobResources(reg, 1)
  expect_equal(res1, res2)

  if (FALSE) {
    # defaults in conf
    if (interactive())
      conf = getBatchJobsConf()
    else
      conf = BatchJobs:::getBatchJobsConf()

    # FIXME why is this even a test?
    # we are setting invalid resources which should always trigger an error
    conf$default.resources = list(walltime=1, memory=2, xxx=3)
    reg = makeTestRegistry()
    batchMap(reg, function(i) getResources(), 1)
    submitJobs(reg, resources=list(memory=200))
    waitForJobs(reg)
    res = list(walltime=1, memory=20, xxx=3)
    expect_equal(loadResult(reg, 1), res)
    expect_equal(getJobResources(reg)[[1]], res)
    conf$default.resources = list()
  }
})
