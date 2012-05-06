context("database")

if (interactive()) {
  
test_that("database", {  
  err.msg = "error with 'quotes'"
  reg = makeTestRegistry()
  batchMap(reg, identity, 1)
  dbSendMessage(reg, dbMakeMessageError(reg, 1, err.msg=err.msg))
  
  reg = makeTestRegistry()
  f = function(x) stop(err.msg)
  batchMap(reg, f, 1)
  submitJobs(reg)
  expect_output(showStatus(reg), "Status for jobs: 1")
})

}
