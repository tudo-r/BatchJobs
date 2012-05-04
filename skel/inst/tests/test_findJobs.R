context("findJobs")

test_that("findJobs", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 10:13)
  ids = findJobs(reg, pars=quote(.arg1 > 10))
  expect_equal(ids, 2:4)

  reg = makeTestRegistry()
  batchMap(reg, function(x) x, x=10:13)
  ids = findJobs(reg, pars=quote(x > 10))
  expect_equal(ids, 2:4)
  
  reg = makeTestRegistry()
  batchExpandGrid(reg, function(x,y) x*y, x=1:2, y=10:11)
  ids = findJobs(reg, pars=quote(.arg1$x == 2))
  expect_true(length(ids) == 2)
  ids = findJobs(reg, pars=quote(.arg1$x == 1 && .arg1$y == 11))
  expect_true(length(ids) == 1)
})
