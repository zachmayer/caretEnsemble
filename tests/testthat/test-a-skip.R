context("Test skips are working correctly")
test_that("Skips work correctly", {
  skip("Basic skip failed")
})
test_that("Skips work correctly", {
  skip_on_travis()
})
test_that("Skips work correctly", {
  skip_on_cran()
})
