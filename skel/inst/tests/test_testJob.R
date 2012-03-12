context("testJob")

if (interactive()) {
  
test_that("testJob", {  
  reg = makeTestRegistry()
  f = function() 1
  id = 1L
  addJob(reg, makeJob(id=id, fun=f, pars=list(), seed=reg$seed)) 
  res = testJob(reg, id)
  expect_equal(res, 1)
  ids = findMissingResults(reg)
  expect_equal(ids, id)
  expect_output({
    df = showStatus(reg)
  }, "Status for jobs: 1")
  expect_true(is.na(df$submitted))
  expect_true(is.na(df$started))
  expect_true(is.na(df$done))
  expect_true(is.na(df$error))
  expect_true(is.na(df$submitted))
  
  reg = makeTestRegistry()
  f = function() {library(xxxxx);1}
  job = addJob(reg, makeJob(id=id, fun=f, pars=list(), seed=reg$seed)) 
  res = testJob(reg, id)
  expect_true(is.null(res))
  ids = findMissingResults(reg)
  expect_equal(ids, id)
  expect_output({
    df = showStatus(reg)
  }, "Status for jobs: 1")
  expect_true(is.na(df$submitted))
  expect_true(is.na(df$started))
  expect_true(is.na(df$done))
  expect_true(is.na(df$error))
  expect_true(is.na(df$submitted))
})

}
