context("batchMapQuick")

test_that("batchMapQuick", {
  reg = batchMapQuick(function(x) x^2, 1:3, file.dir = getTempDir())
  waitForJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, (1:3)^2, check.attributes = FALSE)

  reg = batchMapQuick(function(x,y,z) (x+y)*z, 1:3, 4:6, more.args = list(z = 2), file.dir = getTempDir())
  waitForJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, ((1:3) + (4:6))*2, check.attributes = FALSE)

  reg = batchMapQuick(identity, 1:3, packages = "MASS", chunk.size = 2, file.dir = getTempDir())
  waitForJobs(reg)
  y = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_equal(y, 1:3, check.attributes = FALSE)

  reg = batchMapQuick(identity, 1:3, inds = c(1,3), file.dir = getTempDir())
  waitForJobs(reg)
  expect_equal(findDone(reg), c(1,3))
  y = sapply(c(1,3), function(id) loadResult(reg, id))
  expect_equal(y, c(1,3), check.attributes = FALSE)

  reg = batchMapQuick(identity, file.dir = getTempDir())
  waitForJobs(reg)
  expect_equal(getJobNr(reg), 0L)
})

test_that("batchMapQuick chunks properly", {
  prev = getOption("BatchJobs.verbose", TRUE)
  options(BatchJobs.verbose = TRUE)
  expect_message({
    reg = batchMapQuick(identity, 1:4, chunk.size = 2, file.dir = getTempDir())
  }, "Submitting 2 chunks / 4 jobs")
  options(BatchJobs.verbose = prev)
  waitForJobs(reg)
})
