context("sqlquotes")

test_that("sqlquotes", {
  reg = makeTestRegistry()
  f = function(x) stop("Quoting is 'on'")
  batchMap(reg, f, 1:2)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(findErrors(reg), 1:2)

  reg = makeTestRegistry()
  f = function(x) stop('Quoting is "on"')
  batchMap(reg, f, 1:2)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(findErrors(reg), 1:2)

  reg = makeTestRegistry()
  f = function(x) stop("Some special chars: \n{}\r\t(),;")
  batchMap(reg, f, 1:2)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(findErrors(reg), 1:2)
})
