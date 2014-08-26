context("sanitizePath")

test_that("sanitizePath", {
  paths = c(tempfile(), "..", "~")
  expect_identical(sanitizePath(paths), paths)

  ss = 1:(2L + !isWindows())
  expect_equal(sanitizePath(paths[ss], normalize = TRUE) == paths, c(TRUE, FALSE, FALSE)[ss])

  path = "c:\\temp"
  expect_false(sanitizePath(path, normalize = TRUE) == path)
  expect_false(sanitizePath(path, normalize = FALSE) == path)

  path = file.path(tempdir(), "..", fsep = "/")
  expect_false(sanitizePath(path, normalize = TRUE) == path)
  expect_true(sanitizePath(path, normalize = FALSE) == path)
})
