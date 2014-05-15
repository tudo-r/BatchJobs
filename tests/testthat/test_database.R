context("database")

test_that("database", {
  err.msg = "error with 'quotes'"
  reg = makeTestRegistry()
  batchMap(reg, identity, 1)
  BatchJobs:::dbSendMessage(reg, BatchJobs:::dbMakeMessageError(reg, 1, err.msg=err.msg))
  expect_equal(unname(getErrorMessages(reg)), "error with \"quotes\"")
})

test_that("dbSelectWithIds works", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  query = sprintf("SELECT job_id from %s_job_status", reg$id)
  jids = BatchJobs:::dbSelectWithIds(reg, query, where=TRUE, limit=2)$job_id
  expect_equal(jids, 1:2)

  submitJobs(reg, 1:2)
  waitForJobs(reg)
  query = sprintf("SELECT job_id from %s_job_status where submitted IS NOT NULL", reg$id)
  jids = BatchJobs:::dbSelectWithIds(reg, query, where=FALSE)$job_id
  expect_equal(jids, 1:2)
  jids = BatchJobs:::dbSelectWithIds(reg, query, ids=1:3, where=FALSE)$job_id
  expect_equal(jids, 1:2)
  jids = BatchJobs:::dbSelectWithIds(reg, query, where=FALSE, limit=1)$job_id
  expect_equal(jids, 1)
  jids = BatchJobs:::dbSelectWithIds(reg, query, ids=1:3, where=FALSE, limit=1)$job_id
  expect_equal(jids, 1)
  jids = BatchJobs:::dbSelectWithIds(reg, query, ids=2:3, where=FALSE)$job_id
  expect_equal(jids, 2)

  reg = makeTestRegistry()
  batchMap(reg, identity, 1:4)
  submitJobs(reg, 3:4)
  waitForJobs(reg)
  query = sprintf("SELECT job_id from %s_job_status where submitted IS NOT NULL", reg$id)
  jids = BatchJobs:::dbSelectWithIds(reg, query, ids=1:4, where=FALSE,)$job_id
  expect_equal(jids, 3:4)
})
