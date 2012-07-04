# FIXME: can we find a way to generate log files in unit tests?
context("grepLogs")

test_that("grepLogs", {
  #.... no logs for unittests :(
  # reg = makeTestRegistry()
  # f = function(x) {
  #   if (x %% 2 == 0) warning(x) else x
  # }
  # batchMap(reg, f, 1:10)
  # submitJobs(reg)
  # expect_equal(grepLogs(reg), 1:5*2L)
  # expect_equal(grepLogs(reg, ids=1:2), 2L)
  # expect_equal(grepLogs(reg, pattern = "xxx_nomatch_xxx"), integer(0))
})
