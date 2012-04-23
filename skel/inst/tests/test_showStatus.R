context("showStatus")

test_that("showStatus", {
  reg = makeTestRegistry()
  f = function(x) {
    if (x < 3)
      stop()
    else {
      Sys.sleep(2)
      x
    }
  }
  batchMap(reg, f, 1:3)
  submitJobs(reg)
  expect_output({
    showStatus(reg)
  }, "Status for jobs: 3")
  expect_output({
    showStatus(reg, getJobIds(reg)[2:3])
  }, "Status for jobs: 2")
})


test_that("showStatus only shows some errors", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) stop(), 1:3)
  submitJobs(reg)
  expect_output({
    showStatus(reg, errors=1)
  }, "Showing first 1 errors:")
})

test_that("showStatus works with empty id vector", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) stop(), 1:3)
  submitJobs(reg)
  expect_output({
    showStatus(reg, integer(0L))
  }, "Status for jobs: 0")
})
