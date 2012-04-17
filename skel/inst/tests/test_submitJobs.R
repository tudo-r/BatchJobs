context("submitJobs")

if (interactive()) {

test_that("submitJobs", {
  reg = makeTestRegistry()
  f = function(x) x
  id = 1L
  job = addJob(reg, makeJob(id=id, fun=f, pars=list(x=123), seed=reg$seed))
  submitJobs(reg)
  y = loadResult(reg, id)
  expect_equal(y, 123)
})

test_that("submitJobs", {
  reg = makeTestRegistry(multiple.result.files=TRUE)
  f = function(x) x
  id = 1L
  job = addJob(reg, makeJob(id=id, fun=f, pars=list(x=123), seed=reg$seed))
  submitJobs(reg)
  y = loadResult(reg, id)
  expect_equal(y, 123)
})

}

