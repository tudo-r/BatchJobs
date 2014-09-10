context("source registry files")

test_that("source registry files", {
  mywd = file.path(getWorkDir(), "myworkdir")
  dir.create(mywd, recursive = TRUE, showWarnings = FALSE)
  setwd(mywd)
  src.dir.subdir = c(file.path(mywd, "unittest-sources-subdir"), "unittest-sources-subdir-relative")
  x = lapply(src.dir.subdir, dir.create, recursive = TRUE, showWarnings = FALSE)
  src.dir.parent = c(file.path(dirname(mywd), "unittest-sources-parent"), "../unittest-sources-parent-relative")
  x = lapply(src.dir.parent, dir.create, recursive = TRUE, showWarnings = FALSE)

  for (src.dir in c(src.dir.subdir, src.dir.parent)) {
    cat("xxx = 123", file = file.path(src.dir, "test.R"))
    reg = makeTestRegistry(src.dir = src.dir, work.dir = mywd)
    expect_true(exists("xxx", envir = .GlobalEnv))
    rm(list = "xxx", envir = .GlobalEnv)
    loadRegistry(reg$file.dir)
    expect_true(exists("xxx", envir = .GlobalEnv))
    rm(list = "xxx", envir = .GlobalEnv)

    batchMap(reg, function(i) i + xxx, i = 1:3)
    submitJobs(reg)
    waitForJobs(reg)
    res = loadResults(reg, simplify = TRUE, use.names = "none")
    expect_equal(res, 123 + 1:3)
  }


  src.files.subdir = file.path(src.dir.subdir, "test.R")
  src.files.parent = file.path(src.dir.parent, "test.R")

  for (src.files in c(src.files.subdir, src.files.parent)) {
    reg = makeTestRegistry(src.files = src.files, work.dir = mywd)
    expect_true(exists("xxx", envir = .GlobalEnv))
    rm(list = "xxx", envir = .GlobalEnv)

    loadRegistry(reg$file.dir)
    expect_true(exists("xxx", envir = .GlobalEnv))
    rm(list = "xxx", envir = .GlobalEnv)

    batchMap(reg, function(i) i + xxx, i = 1:3)
    submitJobs(reg)
    waitForJobs(reg)
    res = loadResults(reg, simplify = TRUE, use.names = "none")
    expect_equal(res, 123 + 1:3)
  }
})


test_that("source registry mutators work", {
  mywd = file.path(getWorkDir(), "myworkdir")
  dir.create(mywd, recursive = TRUE, showWarnings = FALSE)
  setwd(mywd)
  src.dir.subdir = c(file.path(mywd, "unittest-sources-subdir"), "unittest-sources-subdir-relative")
  x = lapply(src.dir.subdir, dir.create, recursive = TRUE, showWarnings = FALSE)
  src.dir.parent = c(file.path(dirname(mywd), "unittest-sources-parent"), "../unittest-sources-parent-relative")
  x = lapply(src.dir.parent, dir.create, recursive = TRUE, showWarnings = FALSE)
  src.files.subdir = file.path(src.dir.subdir, "test.R")
  src.files.parent = file.path(src.dir.parent, "test.R")


  reg = makeTestRegistry()
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, character(0L))

  reg = addRegistrySourceFiles(reg, src.files.subdir, src.now = FALSE)
  expect_equal(reg$src.files, sanitizePath(src.files.subdir, make.absolute = FALSE))
  expect_equal(reg$src.dirs, character(0L))

  reg = addRegistrySourceFiles(reg, src.files.subdir, src.now = FALSE)
  expect_equal(reg$src.files, sanitizePath(src.files.subdir, make.absolute = FALSE))
  expect_equal(reg$src.dirs, character(0L))

  reg = addRegistrySourceDirs(reg, src.dir.parent, src.now = FALSE)
  expect_equal(reg$src.files, sanitizePath(src.files.subdir, make.absolute = FALSE))
  expect_equal(reg$src.dirs, sanitizePath(src.dir.parent, make.absolute = FALSE))

  reg = removeRegistrySourceFiles(reg, reg$src.files)
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, sanitizePath(src.dir.parent, make.absolute = FALSE))

  reg = removeRegistrySourceDirs(reg, reg$src.dirs)
  expect_equal(reg$src.files, character(0L))
  expect_equal(reg$src.dirs, character(0L))
})
