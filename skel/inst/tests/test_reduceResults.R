context("reduceResults")

test_that("reduceResults", {
  reg = makeTestRegistry()
  batchMap(reg,  function(x) x^2, 1:3)
  submitJobs(reg)
  y = reduceResults(reg, fun=function(aggr, job, res) aggr+res, init=0L)
  expect_equal(y, sum((1:3)^2))
})


test_that("reduceResults works with empty results", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 1)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, init=0L), 0L)
  submitJobs(reg)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L)), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L), init=0L), 0L)
})

test_that("reduceResults with multiple.result.files", {
  reg = makeTestRegistry(multiple.result.files=TRUE)
  batchMap(reg, function(x) list(foo = x, bar = 1), 1:10)
  submitJobs(reg)

  # quick check for named list as return value of loadResult
  expect_equal(loadResult(reg, 2)$foo, 2)
  expect_equal(loadResult(reg, 2, part="foo"), list(foo=2))
  expect_equal(loadResult(reg, 2, part="bar"), list(bar=1))
  expect_equal(loadResult(reg, 2, part=c("bar", "foo")), list(bar=1, foo=2))
  
  # no part = all parts
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c()), 1:10)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part = c("foo", "bar")), 1:10)

  # wrong part
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part="bar"), NULL)
  # right part
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) c(aggr, res$foo), init = c(), part="foo"), 1:10)
})


test_that("reduceResultsReturnValue", {  
  xs = 1:3
  reg = makeTestRegistry()
  batchMap(reg,  function(x) x^2, xs)
  submitJobs(reg)
  z1 = (1:3)^2

  expect_equal(reduceResultsVector(reg, use.names=FALSE), z1)
  expect_equal(reduceResultsList(reg, use.names=FALSE), as.list(z1))
  expect_equal(reduceResultsMatrix(reg, use.names=FALSE), matrix(z1, ncol=1))
  y = data.frame(z1); colnames(y) = NULL
  expect_equal(reduceResultsDataFrame(reg), y)

  z2 = z1
  names(z2) = xs
  names(z2) = xs
  expect_equal(reduceResultsVector(reg, use.names=TRUE), z2)
  expect_equal(reduceResultsList(reg, use.names=TRUE), as.list(z2))
  y = matrix(z2, ncol=1); rownames(y)=xs; colnames(y) = NULL
  expect_equal(reduceResultsMatrix(reg, use.names=TRUE), y)
  
  reg = makeTestRegistry()
  batchMap(reg, function(x) list(a=x, b=x^2), xs)
  submitJobs(reg)
  
  expect_equal(reduceResultsVector(reg, fun=function(job, res) res$a, use.names=FALSE), xs)
  expect_equal(reduceResultsVector(reg, fun=function(job, res) res$b, use.names=FALSE), xs^2)
  expect_equal(reduceResultsList(reg, fun=function(job, res) res$a, use.names=FALSE), as.list(xs))
  expect_equal(reduceResultsList(reg, fun=function(job, res) res$b, use.names=FALSE), as.list((xs)^2))

  y = cbind(xs, z1); dimnames(y) = NULL
  expect_equal(reduceResultsMatrix(reg, use.names=FALSE), y)
  rownames(y) = xs; colnames(y) = c("a", "b")
  expect_equal(reduceResultsMatrix(reg, use.names=TRUE), y)
  dimnames(y) = NULL; y = t(y)
  expect_equal(reduceResultsMatrix(reg, use.names=FALSE, rows=FALSE), y)
  colnames(y) = xs; rownames(y) = c("a", "b")
  expect_equal(reduceResultsMatrix(reg, use.names=TRUE, rows=FALSE), y)
  
  y = as.data.frame(cbind(xs, z1)); rownames(y) = xs; colnames(y) = c("a", "b")
  expect_equal(reduceResultsDataFrame(reg), y)
  
  reg = makeTestRegistry()
  batchMap(reg, function(i) list(a="a", b="b"), 1:2)
  submitJobs(reg)
  y = data.frame(a=rep("a", 2), b=rep("b", 2), stringsAsFactors=FALSE)
  expect_equal(reduceResultsDataFrame(reg, strings.as.factors=FALSE), y)
  y = data.frame(a=rep("a", 2), b=rep("b", 2), stringsAsFactors=TRUE)
  expect_equal(reduceResultsDataFrame(reg, strings.as.factors=TRUE), y)
})

test_that("reduceResultsReturnValue works with empty results", {  
  xs = integer(0)
  reg = makeTestRegistry()
  batchMap(reg,  function(x) identity, xs)
  submitJobs(reg)

  expect_equal(reduceResultsVector(reg, use.names=FALSE), c())
  expect_equal(reduceResultsList(reg, use.names=FALSE), list())
  expect_equal(reduceResultsMatrix(reg, use.names=FALSE), matrix(0,0,0))
  expect_equal(reduceResultsDataFrame(reg, use.names=FALSE), data.frame())
})



