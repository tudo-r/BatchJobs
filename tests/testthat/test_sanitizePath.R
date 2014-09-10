context("sanitizePath")

test_that("sanitizePath", {
  cS = function(x) gsub("\\", "/", x, fixed = TRUE)

  if (isLinux()) {
    setwd(tempdir())
    paths = c(tempfile(), "..", "~")
    expect_identical(sanitizePath(paths, make.absolute = TRUE), c(paths[1], dirname(tempdir()), path.expand("~")))
    expect_equal(sanitizePath(paths, make.absolute = TRUE) == paths, c(TRUE, FALSE, FALSE))
  } else if (isWindows()) {
    setwd(tempdir())
    paths = c(tempfile(), "..")
    expect_identical(sanitizePath(paths), cS(c(paths[1], dirname(tempdir()))))
    expect_equal(sanitizePath(paths, normalize = TRUE) == cS(paths), c(TRUE, FALSE))
  } else if (isDarwin()) {
    # mac os often has a symlinked tempdir
    setwd(tempdir())
    paths = c(tempfile(), "~")
    expect_identical(sanitizePath(paths, make.absolute = TRUE), cS(c(paths[1], path.expand("~"))))
    expect_equal(sanitizePath(paths, normalize = TRUE) == cS(paths), c(TRUE, FALSE))
  }

  path = "c:\\temp"
  expect_false(sanitizePath(path, make.absolute = FALSE) == path)
  expect_identical(sanitizePath(path, make.absolute = FALSE), cS(path))
  expect_identical(sanitizePath(path, make.absolute = TRUE), cS(path))

  path = file.path(tempdir(), "..", fsep = "/")
  expect_false(sanitizePath(path, make.absolute = TRUE, normalize.absolute = TRUE) == cS(path))
  expect_identical(sanitizePath(path, make.absolute = FALSE), cS(path))
  if (!isDarwin())
    expect_identical(sanitizePath(path, make.absolute = TRUE, normalize.absolute = TRUE), cS(dirname(dirname(path))))
})
