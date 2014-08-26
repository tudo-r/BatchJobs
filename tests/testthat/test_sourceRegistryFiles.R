context("source registry files")

test_that("source registry files", {
  reg = makeTestRegistry()
  p1 = "unittest-sources"
  p2 = file.path(getWorkDir(), p1)
  dir.create(p2, recursive = TRUE, showWarnings = FALSE)
  cat("xxx = 123", file = file.path(p2, "test.R"))
  reg = makeTestRegistry(src.dir = p1)

  expect_true(exists("xxx", envir = .GlobalEnv))

  rm(list = "xxx", envir = .GlobalEnv)
  loadRegistry(reg$file.dir)
  expect_true(exists("xxx", envir = .GlobalEnv))

  batchMap(reg, function(i) i + xxx, i = 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  res = loadResults(reg, simplify = TRUE, use.names = "none")
  expect_equal(res, 123 + 1:3)

  reg = makeTestRegistry(src.files = file.path(p1, "test.R"))
  expect_true(exists("xxx", envir = .GlobalEnv))

  rm(list = "xxx", envir = .GlobalEnv)
  loadRegistry(reg$file.dir)
  expect_true(exists("xxx", envir = .GlobalEnv))

  batchMap(reg, function(i) i + xxx, i = 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  res = loadResults(reg, simplify = TRUE, use.names = "none")
  expect_equal(res, 123 + 1:3)
})


test_that("source registry mutators work", {
  p1 = "unittest-sources"
  p2 = file.path(getWorkDir(), p1)
  dir.create(p2, recursive = TRUE, showWarnings = FALSE)
  cat("xxx = 123", file = file.path(p2, "test.R"))
  p3 = file.path(p1, "test.R")

  reg = makeTestRegistry(src.dir = p1)
  reg = makeTestRegistry()
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, character(0L))
  reg = addRegistrySourceFiles(reg, p3, src.now = FALSE)
  expect_equal(reg$src.files, p3)
  expect_equal(reg$src.dirs, character(0L))
  reg = addRegistrySourceDirs(reg, p1, src.now = FALSE)
  expect_equal(reg$src.files, p3)
  expect_equal(reg$src.dirs, p1)
  reg = removeRegistrySourceFiles(reg, p3)
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, p1)
  reg = removeRegistrySourceDirs(reg, p1)
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, character(0L))
})


