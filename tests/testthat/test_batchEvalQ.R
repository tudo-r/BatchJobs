context("batchEvalQ")

test_that("batchEvalQ", {
  ## Global string, which should be found instead of base::url().
  url <- "http://www.r-project.org"

  ## A single expression to be evaluated
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
  expect_identical(length(ids), 1L)

  submitJobs(reg)
  waitForJobs(reg)

  res = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_identical(length(res), length(ids))
  expect_identical(res, url)


  ## Multiple expressions to be evaluated
  exprs <- list(
    A = substitute({ x <- 2; x^2 }),
    B = substitute({ x <- 3; x^2 }),
    C = substitute({ x <- 4; x^2 })
  )
  print(exprs)

  reg = makeTestRegistry()
  ids = batchEvalQ(reg, exprs=exprs)
  expect_identical(length(ids), 3L)

  submitJobs(reg)
  waitForJobs(reg)

  res = sapply(getJobIds(reg), function(id) loadResult(reg, id))
  expect_identical(length(res), length(ids))
  expect_identical(res, c(4, 9, 16))
})
