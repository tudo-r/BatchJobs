context("exports")

test_that("exports", {
          # library(BatchJobs)
          # library(testthat)
          # source("helpers.R")
  reg = makeTestRegistry()
  p = file.path(reg$file.dir, "exports")
  save2(exported_x = 1:3, file = file.path(p, "x.RData"))
  loadExports(reg)
  expect_true(exists("exported_x", where=.GlobalEnv))
  expect_equal(exported_x, 1:3)

  batchMap(reg, function(i) exported_x[i], i = 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  res = loadResults(reg, simplify=TRUE, use.names="none")
  expect_equal(res, 1:3)
})
