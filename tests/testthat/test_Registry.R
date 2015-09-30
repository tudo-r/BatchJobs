context("Registry")

test_that("registry with read.only set works", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)
  waitForJobs(reg)

  reg$read.only = TRUE

  expect_error(batchMap(reg, function(x) x^2, 1:3), "read-only")
  expect_error(submitJobs(reg), "read-only")
  expect_error(removeRegistrySourceFiles(reg), "read-only")
  expect_error(killJobs(reg), "read-only")
  expect_error(removeRegistryPackages(reg), "read-only")
  expect_error(resetJobs(reg), "read-only")
  expect_error(sweepRegistry(reg), "read-only")

  expect_data_frame(getStatus(reg))
  expect_integer(findJobs(reg))
  expect_list(getJobs(reg), types = "Job")
  expect_class(getJob(reg, 1), "Job")
  expect_true(sourceRegistryFiles(reg))
  expect_numeric(reduceResultsVector(reg))
  expect_numeric(reduceResults(reg, ids = 1:3, fun = function(job, res, aggr) c(aggr, res)))
  expect_numeric(filterResults(reg, fun = function(job, res) TRUE))
  expect_numeric(testJob(reg, id = 1, external = FALSE))
})
