context("getLogFiles")

test_that("getLogFiles", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:10)
  ids = getJobIds(reg)
  ch = chunkJobs(ids[1:8], chunk.size=4)
  submitJobs(reg, ch)
  submitJobs(reg, ids[9:10])
  fs = getLogFiles(reg, ids)
  expect_equal(length(unique(fs)), 4)
  expect_true(str_detect(fs[9], "9"))
  expect_true(str_detect(fs[10], "10"))

  expect_equal(getLogFiles(reg, integer(0L)), character(0L))
})

