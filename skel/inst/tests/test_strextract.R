context("strextract")

if (interactive()) {

test_that("strextract", {
  x = c( "xxx 12 xxx", " 12 12 xx 12")
  expect_equal(strextract(x, "\\d+"), rep("12", 2))
  x = "240935 0.00000 sleep 60   matthias     qw    04/03/2012 15:45:54"
  expect_equal(strextract(x, "\\d+"), "240935")

  expect_true(length(strextract("", "\\d+")) == 0)
  expect_true(length(strextract(character(0), "\\d+")) == 0)
  expect_true(length(strextract(NULL, "\\d+")) == 0)
})

}
