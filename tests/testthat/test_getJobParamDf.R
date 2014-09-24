context("getJobParamDf")

test_that("getJobParamDf", {
  reg = makeTestRegistry()
  grid = batchExpandGrid(reg, function(x,y) x*y, x = 1:3, y = 5)
  expect_equal(getJobParamDf(reg), grid)
  expect_equal(getJobParamDf(reg, ids = 2:3), grid[2:3,])
})

