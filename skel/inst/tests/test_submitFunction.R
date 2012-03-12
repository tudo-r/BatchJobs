context("submitFunCall")

test_that("submitFunCall", {  
  td = tempdir()
  fp = file.path(td, "result.RData")
  submitFunCall("foo", fun=function(x) x+1, pars=list(x=2), file=fp)
  load(fp)
  expect_equal(result, 3)
})

