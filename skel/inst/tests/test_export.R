context("exports")

test_that("exports", {
  reg = makeTestRegistry()
  p = BatchJobs:::getExportDir(reg$file.dir)
  save2(exported_x = 1:3, file = file.path(p, "x.RData"))
  expect_false(exists("exported_x", where=.GlobalEnv))
  loadExports(reg)
  expect_true(exists("exported_x", where=.GlobalEnv))
  expect_equal(exported_x, 1:3)
})
