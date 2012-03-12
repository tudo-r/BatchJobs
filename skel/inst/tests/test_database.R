context("database")

if (interactive()) {
  
test_that("database", {  
  err.msg = "error with 'quotes'"
  reg = makeTestRegistry()
  f = function() 1
  id = 1L
  addJob(reg, makeJob(id=id, fun=f, pars=list(), seed=reg$seed))  
  dbSendMessage(reg, dbMakeMessageError(reg, id, err.msg=err.msg))
  reg = makeTestRegistry()
  f = function(x) stop(err.msg)
  batchMap(reg, f, 1)
  submitJobs(reg)
  expect_output(showStatus(reg), "Status for jobs: 1")
})

}
