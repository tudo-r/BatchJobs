context("chunkJobs")

test_that("chunkJobs", {
  reg = makeTestRegistry()
  f = function(i) i
  batchMap(reg, f, 1:24)
  ids = getJobIds(reg)

  n = sapply(chunkJobs(ids, chunk.size = 2), length)
  expect_equal(length(n), 12)
  expect_true(all(n == 2))

  n = sapply(chunkJobs(ids, n.chunks = 5), length)
  expect_equal(length(n), 5)
  expect_true(all(n == c(5, 5, 5, 5, 4)))

  expect_error(chunkJobs(ids, chunk.size = 1, n.chunks = 3))
  expect_error(chunkJobs(ids, chunk.size=1:2))
  expect_error(chunkJobs(ids, n.chunks=list()))
  # convertIntegers cannot work with NULL
  #expect_error(chunkJobs(NULL, 2))
  
  chunks = chunkJobs(ids, chunk.size = 10)
  expect_true(all(ids %in% unlist(chunks)))
})
