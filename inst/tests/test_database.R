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
  expect_output(showStatus(reg), "Status for 1 jobs")
})

test_that("dbSelectWithIds works", {  
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  query = sprintf("SELECT job_id from %s_job_status", reg$id)
  jids = dbSelectWithIds(reg, query, where=TRUE, limit=2)$job_id
  expect_equal(jids, 1:2)
  
  submitJobs(reg, 1:2)
  query = sprintf("SELECT job_id from %s_job_status where submitted IS NOT NULL", reg$id)
  jids = dbSelectWithIds(reg, query, where=FALSE)$job_id
  expect_equal(jids, 1:2)
  jids = dbSelectWithIds(reg, query, ids=1:3, where=FALSE)$job_id
  expect_equal(jids, 1:2)
  jids = dbSelectWithIds(reg, query, where=FALSE, limit=1)$job_id
  expect_equal(jids, 1)
  jids = dbSelectWithIds(reg, query, ids=1:3, where=FALSE, limit=1)$job_id
  expect_equal(jids, 1)
  jids = dbSelectWithIds(reg, query, ids=2:3, where=FALSE)$job_id
  expect_equal(jids, 2)
  
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:4)
  submitJobs(reg, 3:4)
  query = sprintf("SELECT job_id from %s_job_status where submitted IS NOT NULL", reg$id)
  jids = dbSelectWithIds(reg, query, ids=1:4, where=FALSE,)$job_id
  expect_equal(jids, 3:4)
})


}
