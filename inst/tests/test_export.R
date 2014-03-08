context("exports")

test_that("exports", {
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


test_that("exports work with external testJob", {
  reg = makeTestRegistry()
  p = file.path(reg$file.dir, "exports")
  save2(exported_x = 1:3, file = file.path(p, "x.RData"))
  batchMap(reg, function(i) exported_x[i], i = 1:2)
  res = testJob(reg, 2L, external=TRUE)
  expect_equal(res, 2L)
})
