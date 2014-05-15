context("reduceResults")

test_that("reduceResults", {
  reg = makeTestRegistry()
  batchMap(reg,  function(x) x^2, 1:3)
  submitJobs(reg)
  waitForJobs(reg)
  y = reduceResults(reg, fun=function(aggr, job, res) aggr+res, init=0L)
  expect_equal(y, sum((1:3)^2))
})


test_that("reduceResults works with empty results", {
  reg = makeTestRegistry()
  batchMap(reg, function(x) x, 1)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, init=0L), 0L)
  submitJobs(reg)
  waitForJobs(reg)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L)), NULL)
  expect_equal(reduceResults(reg, fun=function(aggr, job, res) 1, ids=integer(0L), init=0L), 0L)
})

test_that("reduceResults works with imputations", {
  reg = makeTestRegistry()
  batchMap(reg, identity, 1:5)
  submitJobs(reg, c(1:2, 4:5))
  waitForJobs(reg)
  expect_equal(reduceResults(reg, ids=1:5, fun=function(aggr, job, res) aggr+res, impute.val=3), 15)
  expect_equal(reduceResults(reg, ids=1:5, fun=function(aggr, job, res) sum(aggr, res, na.rm=TRUE), impute.val=NA), 12)
})

test_that("reduceResults with multiple.result.files", {
  reg = makeTestRegistry(multiple.result.files=TRUE)
  batchMap(reg, function(x) list(foo = x, bar = 1), 1:10)
  submitJobs(reg)
  waitForJobs(reg)

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
  waitForJobs(reg)
  z1 = (1:3)^2

  expect_equal(reduceResultsVector(reg, use.names="none"), z1)
  expect_equal(reduceResultsList(reg, use.names="none"), as.list(z1))
  expect_equal(reduceResultsMatrix(reg, use.names="none"), matrix(z1, ncol=1))

  # data.frame w/o colnames is nothing we want to test ...
  # y = data.frame(z1); colnames(y) = NULL
  y = data.frame(z1); colnames(y) = "X"; rownames(y) = as.character(rownames(y))
  expect_equal(reduceResultsDataFrame(reg), y)

  z2 = z1
  names(z2) = xs
  names(z2) = xs
  expect_equal(reduceResultsVector(reg, use.names="ids"), z2)
  expect_equal(reduceResultsList(reg, use.names="ids"), as.list(z2))
  y = matrix(z2, ncol=1); rownames(y)=xs; colnames(y) = NULL
  expect_equal(reduceResultsMatrix(reg, use.names="ids"), y)

  reg = makeTestRegistry()
  batchMap(reg, function(x) list(a=x, b=x^2), xs)
  submitJobs(reg)
  waitForJobs(reg)

  expect_equal(reduceResultsVector(reg, fun=function(job, res) res$a, use.names="none"), xs)
  expect_equal(reduceResultsVector(reg, fun=function(job, res) res$b, use.names="none"), xs^2)
  expect_equal(reduceResultsList(reg, fun=function(job, res) res$a, use.names="none"), as.list(xs))
  expect_equal(reduceResultsList(reg, fun=function(job, res) res$b, use.names="none"), as.list((xs)^2))

  y = cbind(xs, z1); dimnames(y) = NULL
  expect_equal(reduceResultsMatrix(reg, use.names="none"), y)
  rownames(y) = xs; colnames(y) = c("a", "b")
  expect_equal(reduceResultsMatrix(reg, use.names="ids"), y)
  dimnames(y) = NULL; y = t(y)
  expect_equal(reduceResultsMatrix(reg, use.names="none", rows=FALSE), y)
  colnames(y) = xs; rownames(y) = c("a", "b")
  expect_equal(reduceResultsMatrix(reg, use.names="ids", rows=FALSE), y)

  y = data.frame(xs, z1); rownames(y) = as.character(xs); colnames(y) = c("a", "b")
  expect_equal(reduceResultsDataFrame(reg), y)

  reg = makeTestRegistry()
  batchMap(reg, function(i) list(a="a", b="b"), 1:2)
  submitJobs(reg)
  waitForJobs(reg)
  y = data.frame(a=rep("a", 2), b=rep("b", 2), stringsAsFactors=FALSE)
  rownames(y) = as.character(rownames(y))
  expect_equal(reduceResultsDataFrame(reg, strings.as.factors=FALSE), y)
  y = data.frame(a=rep("a", 2), b=rep("b", 2), stringsAsFactors=TRUE)
  rownames(y) = as.character(rownames(y))
  expect_equal(reduceResultsDataFrame(reg, strings.as.factors=TRUE), y)
})

test_that("reduceResultsReturnValue works with empty results", {
  xs = integer(0)
  reg = makeTestRegistry()
  batchMap(reg,  function(x) identity, xs)
  submitJobs(reg)
  waitForJobs(reg)

  expect_equal(reduceResultsVector(reg, use.names="none"), c())
  expect_equal(reduceResultsList(reg, use.names="none"), list())
  expect_equal(reduceResultsMatrix(reg, use.names="none"), matrix(0, nrow=0,ncol=0))
  expect_equal(reduceResultsDataFrame(reg, use.names="none"), data.frame())
})

test_that("reduceResultsReturnValue works with imputations", {
  reg = makeTestRegistry()
  ids = batchMap(reg, identity, 1:5)
  submitJobs(reg, 1:4)
  waitForJobs(reg)

  res = c(1:4, NA_real_)
  expect_equal(reduceResultsVector(reg, ids, impute.val=NA_real_), setNames(res, 1:5))
  expect_equal(reduceResultsList(reg, ids, impute.val=NA_real_), setNames(as.list(res), 1:5))
  expect_equal(reduceResultsMatrix(reg, ids, impute.val=NA_real_), setRowNames(matrix(res), 1:5))
  expect_equal(reduceResultsDataFrame(reg, ids, impute.val=NA_real_, use.names="none"), data.frame(X = res))
})



test_that("reduceResultsReturnValue works with other args", {
  reg = makeTestRegistry()
	xs = 1:3
	batchMap(reg, identity, xs)
	submitJobs(reg)
  waitForJobs(reg)
	z = reduceResultsMatrix(reg, fun = function(job, res, y) (res-y)^2, y = 1, use.names = "none")
  expect_equal(z[,1], (xs-1)^2)
})

test_that("reduceResultsReturnValue works with job names", {
  reg = makeTestRegistry()
  ns = letters[1:3]
	xs = setNames(1:3, ns)
	batchMap(reg, identity, xs, use.names=TRUE)
	submitJobs(reg)
  waitForJobs(reg)

  expect_equal(names(reduceResultsList(reg, use.names="names")), ns)
  expect_equal(names(reduceResultsVector(reg, use.names="names")), ns)
  expect_equal(rownames(reduceResultsDataFrame(reg, use.names="names")), ns)
  expect_equal(rownames(reduceResultsMatrix(reg, use.names="names")), ns)
  expect_equal(colnames(reduceResultsMatrix(reg, use.names="names", rows=FALSE)), ns)
})
