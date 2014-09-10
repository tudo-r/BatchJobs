context("sweepRegistry")

test_that("sweepRegistry", {
  countFiles = function(reg, type) {
    fun = switch(type,
      scripts = BatchJobs:::getRScriptFilePath,
      logs = BatchJobs:::getLogFilePath
    )
    fs = sapply(getJobIds(reg), fun, reg = reg)
    sum(sapply(fs, file.exists))
  }

  reg = makeTestRegistry()
  batchMap(reg, function(x) x^2, 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(countFiles(reg, "scripts"), 3L)
  expect_equal(countFiles(reg, "logs"), 3L)
  sweepRegistry(reg, "scripts")
  expect_equal(countFiles(reg, "scripts"), 0L)
  expect_equal(countFiles(reg, "logs"), 3L)
  sweepRegistry(reg, "logs")
  expect_equal(countFiles(reg, "scripts"), 0L)
  expect_equal(countFiles(reg, "logs"), 0L)
})



