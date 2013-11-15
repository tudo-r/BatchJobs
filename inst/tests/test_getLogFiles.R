context("getLogFiles")

test_that("getLogFiles", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:10)
  ids = getJobIds(reg)
  ch = chunk(ids[1:8], chunk.size=4)
  submitJobs(reg, ch)
  submitJobs(reg, ids[9:10])
  waitForJobs(reg)
  fs = getLogFiles(reg, ids)
  expect_equal(length(unique(fs)), 4)
  expect_true(grepl("9", fs[9]))
  expect_true(grepl("10", fs[10]))

  expect_equal(getLogFiles(reg, integer(0L)), character(0L))
})

