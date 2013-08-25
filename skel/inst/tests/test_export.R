context("exports")

test_that("exports", {
  # TODO more tests
  reg = makeTestRegistry()
  expect_equal(batchExport(reg, x = 1L, y = 2L), c("x", "y"))
  expect_equal(unname(unlist(batchListExported(reg))), c("x", "y"))
  expect_false("x" %in% ls(.GlobalEnv))
  expect_equal(batchImport(reg), c("x", "y"))
  expect_true(all(c("x", "y") %in% ls(.GlobalEnv)))
  expect_equal(batchUnexport(reg, "x"), "x")
  expect_equal(unname(unlist(batchListExported(reg))), "y")
})
