context("packages")

test_that("packages", {
  reg = makeTestRegistry(packages = "MASS")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS")))
  reg = addRegistryPackages(reg, "testthat")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "MASS", "testthat")))
  reg = removeRegistryPackages(reg, "MASS")
  expect_true(is.list(reg$packages) && setequal(names(reg$packages), c("BatchJobs", "testthat")))
})


