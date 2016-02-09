context("packages")

test_that("packages", {
  reg = makeTestRegistry(packages = "MASS")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS")))

  reg = addRegistryPackages(reg, "testthat")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "testthat")))

  reg = addRegistryPackages(reg, "checkmate")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "testthat", "checkmate")))

  reg = removeRegistryPackages(reg, "testthat")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "checkmate")))

  reg = removeRegistryPackages(reg, "MASS")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "checkmate")))

  reg = setRegistryPackages(reg, c("MASS", "checkmate"))
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "checkmate")))

  reg = addRegistryPackages(reg, "BatchJobs")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "checkmate")))
})
