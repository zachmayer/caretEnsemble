context("Test skips are working correctly")
test_that("Skips work correctly", {
  skip("Basic skip failed")
  expect_equal(1, 0)
})
test_that("Skips work correctly on CRAN", {
  skip_on_cran()
  expect_equal(1, 1)
})
