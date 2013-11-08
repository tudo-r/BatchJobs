context("source registry files")

test_that("source registry files", {
  p = tempdir()
  dir.create(file.path(p, "src"), recursive=TRUE)
  cat("xxx = 123", file = file.path(p, "src", "test.R"))

  reg = makeTestRegistry(work.dir = tempdir(), src.dirs = "src")
  expect_true(exists("xxx", envir=.GlobalEnv))

  rm(list = "xxx", envir=.GlobalEnv)
  loadRegistry(reg$file.dir)
  expect_true(exists("xxx", envir=.GlobalEnv))

  batchMap(reg, function(i) i + xxx, i = 1:3)
  submitJobs(reg)
  res = loadResults(reg, simplify=TRUE, use.names="none")
  expect_equal(res, 123 + 1:3)
})
