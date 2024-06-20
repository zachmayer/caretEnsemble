context("Code is high quality and lint free")
test_that("Code Lint", {
  skip_on_cran()
  skip_if_not_installed("lintr")

  errors <- lintr::lint_package("../../")
  expect_true(
    length(errors) == 0,
    paste0("There are ", length(errors), " errors. Please run `lintr::lint_package(\".\")` to see the errors.")
  )
})
