context("system3")

if (interactive()) {

test_that("system3", {  
  d = tempfile()
  dir.create(d)
  fn = file.path(d, "foo.bar")
  file.create(fn)
  
  # no error
  res = system3("ls", d)
  expect_equal(res, list(exit.code=0L, output=as.character(NA)))
  res = system3("ls", d, stdout=TRUE, stderr=TRUE)
  expect_equal(res, list(exit.code=0L, output="foo.bar"))
  
  # wrong command
  res = system3("xxx", stop.on.exit.code=FALSE)
  expect_equal(res, list(exit.code=127L, output=as.character(NA)))
  expect_error(system3("xxx", stop.on.exit.code=TRUE), 
    "Command: xxx ; exit code: 127; output: NA")
  
  # exit code
  res = system3("ls", "xxx", stop.on.exit.code=FALSE)
  expect_equal(res, list(exit.code=2L, output=as.character(NA)))
  res = system3("ls", "xxx", stdout=TRUE, stderr=TRUE, stop.on.exit.code=FALSE)
  msg = "ls: cannot access xxx: No such file or directory"
  expect_equal(res, list(exit.code=2L, output=msg))
  expect_error(system3("ls", "xxx", stdout=TRUE, stderr=TRUE, stop.on.exit.code=TRUE),
     sprintf("Command: ls xxx; exit code: 2; output: %s", msg))
})

}
