context("makeRegistry")

test_that("makeRegistry", {
  reg = makeTestRegistry()
  expect_true(inherits(reg, "Registry"))
  expect_true(is.list(reg))
  expect_true(file.exists(reg$file.dir))
  expect_true(file.exists(file.path(reg$file.dir, "BatchJobs.db")))
  expect_output(print(reg), "Job registry")
  df = BatchJobs:::dbGetJobStatusTable(reg)
  expect_true(is.data.frame(df) && nrow(df) == 0 && ncol(df) == 13)
})


test_that("makeRegistry checks id", {
  expect_error(makeRegistry(id="runit-files"),  "comply with")
  expect_error(makeRegistry(id="-files"),  "comply with")
})


test_that("makeRegistry checks packs", {
  makeTestRegistry(packages="base")
  expect_error(makeTestRegistry(packages="foo"),
    "Please install the following packages: foo")
})

test_that("loadRegistry works", {
  reg1 = makeTestRegistry()
  reg2 = loadRegistry(reg1$file.dir)
  expect_is(reg2, "Registry")
})


test_that("torturing makeRegistry/removeRegistry to create registries over and over works", {
  ## This fails (at least) on Windows if we set the working directory
  ## to be the current working directory, i.e. ".".  This can be achieve
  ## using Sys.setenv("R_EXPENSIVE_EXAMPLE_OK"=TRUE).
  for (ii in 1:50) {
    reg = makeTestRegistry()
    expect_true(removeRegistry(reg, ask = "no"))
  }
})
