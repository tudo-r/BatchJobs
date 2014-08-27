context("sanitizePath")

test_that("sanitizePath", {
  cS = function(x) gsub("\\", "/", x, fixed = TRUE)

  if (!isWindows()) {
    setwd(tempdir())
    paths = c(tempfile(), "..", "~")
    expect_identical(sanitizePath(paths), c(paths[1], dirname(tempdir()), path.expand("~")))
    expect_equal(sanitizePath(paths, normalize = TRUE) == paths, c(TRUE, FALSE, FALSE))
  } else {
    setwd(tempdir())
    paths = c(tempfile(), "..")
    expect_identical(sanitizePath(paths), cS(c(paths[1], dirname(tempdir()))))
    expect_equal(sanitizePath(paths, normalize = TRUE) == cS(paths), c(TRUE, FALSE))
  }

  path = "c:\\temp"
  expect_false(sanitizePath(path, normalize = FALSE) == path)
  expect_identical(sanitizePath(path, normalize = FALSE), cS(path))
  expect_identical(sanitizePath(path, normalize = TRUE), cS(path))

  path = file.path(tempdir(), "..", fsep = "/")
  expect_false(sanitizePath(path, normalize = TRUE) == cS(path))
  expect_identical(sanitizePath(path, normalize = FALSE), cS(path))
  expect_identical(sanitizePath(path, normalize = TRUE), cS(dirname(dirname(path))))
})
