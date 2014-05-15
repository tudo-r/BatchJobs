context("getJob")

test_that("getJob", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  ids = getJobIds(reg)
  j = getJob(reg, ids[2])
  expect_equal(j$id, ids[2])
  expect_equal(j$pars, list(2))
  expect_is(j, "Job")

  js = getJobs(reg, ids[2:3])
  expect_is(js, "list")
  expect_equal(length(js), 2)
  expect_is(js[[1]], "Job")
  expect_is(js[[2]], "Job")
  expect_equal(js[[1]]$id, ids[2])
  expect_equal(js[[2]]$id, ids[3])
  expect_equal(js[[1]]$pars, list(2))
  expect_equal(js[[2]]$pars, list(3))

  # empty ids
  expect_equal(getJobs(reg, integer(0)), list())
  # bad id
  expect_error(getJob(reg, "xxx"), "integer")
  expect_error(getJobs(reg, -1), "Ids not present in registry: -1")
})

test_that("getJob returns jobs in same order as ids", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:3)
  ids = getJobIds(reg)
  ids = rev(ids)
  js = getJobs(reg, ids)
  expect_equal(ids, extractSubList(js, "id"))
})

