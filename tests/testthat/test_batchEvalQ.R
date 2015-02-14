context("batchEvalQ")

test_that("batchEvalQ", {
  ## Global string, which should be found instead of base::url().
  url <- "http://www.r-project.org"

  ## Expression to be evaluated
  expr <- substitute({
    cat("URL:\n")
    print(url)
    stopifnot(is.character(url), identical(url, "http://www.r-project.org"))
    url
  })
  print(expr)

  reg = makeTestRegistry()
  res = batchExport(reg, li=list(url=url))
  ids = batchEvalQ(reg, exprs=list(expr))

  submitJobs(reg)
  waitForJobs(reg)

  res = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_identical(res, url)
})
