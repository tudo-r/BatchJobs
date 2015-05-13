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

test_that("exports with batchExport and batchUnexport", {
  reg = makeTestRegistry()
  expect_error(batchExport(reg, a = 1, li = list(a = 2)), "more than once")
  expect_equal(batchExport(reg, a = 1, b = 99), c("a", "b"))
  batchMap(reg, function(x) x + a + b, 1)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(loadResult(reg, 1), 101)
  if (interactive()) {
    expect_equal(testJob(reg, 1), 101)
    expect_equal(testJob(reg, 1, external = FALSE), 101)
    expect_equal(testJob(reg, 1, external = TRUE), 101)
  }
  expect_equal(batchUnexport(reg, "a"), "a")
  suppressWarnings(rm(list = "a", envir = .GlobalEnv))
  submitJobs(reg, 1)
  waitForJobs(reg)
  expect_equal(length(findErrors(reg)), 1)
  expect_true(grepl("not found", getErrorMessages(reg, 1), fixed = TRUE))
})

test_that("export: load defined files with loadExports", {
  reg = makeTestRegistry()
  batchExport(reg, x.1 = 4, x.2 = 3, x.3 = 2, x.4 = 1)
  suppressMessages(loadExports(reg, paste0("x.", seq(2, 6, 2))))
  expect_false(exists("x.1", where = .GlobalEnv))
  expect_true(exists("x.2", where = .GlobalEnv))
  expect_false(exists("x.3", where = .GlobalEnv))
  expect_true(exists("x.4", where = .GlobalEnv))
  expect_false(exists("x.6", where = .GlobalEnv))
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
