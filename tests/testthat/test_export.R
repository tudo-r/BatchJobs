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


#FIXME: 
# I currently do not know how to run this test so it does not break R CMD Check
# I get
# >  cannot open file 'startup.Rs': No such file or directory
# 'make test' would work 
# --> run it only in interactive tests for now
if (interactive()) {
test_that("exports work with external testJob", {
  reg = makeTestRegistry()
  p = file.path(reg$file.dir, "exports")
  save2(exported_x = 1:3, file = file.path(p, "x.RData"))
  batchMap(reg, function(i) exported_x[i], i = 1:2)
  res = testJob(reg, 2L, external=TRUE)
  expect_equal(res, 2L)
})

}
