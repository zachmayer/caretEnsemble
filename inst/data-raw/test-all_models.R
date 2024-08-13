
# Load pre-computed test data
all_models <- NULL
load(file.path("R", "sysdata.rda"))

# Load libraries needed
libs <- lapply(all_models, function(x) {
  x$modelInfo$library
})
libs <- sort(unique(unlist(libs)))

# Check libraries are installed
testthat::test_that("All libraries are installed", {
  is_installed <- vapply(libs, function(x) {
    suppressWarnings(suppressMessages(requireNamespace(x, quietly = TRUE)))
  }, logical(1L))
  if (!all(is_installed)) {
    stop("Lib not installed: ", libs[!is_installed], call. = FALSE)
  }
  testthat::expect_true(all(is_installed))
})

# Check that all models can predict
testthat::test_that("Most caret models can predict", {
  for (lib in libs) {
    # Caret needs the libs loaded.
    require(lib, character.only = TRUE) # nolint undesired_function_linter
  }
  testthat::expect_gt(length(all_models), 200L) # About 100 each of class/reg
  pred <- suppressWarnings(suppressMessages(predict(all_models, X)))
  testthat::expect_identical(nrow(pred), nrow(X))
  testthat::expect_identical(ncol(pred), length(all_models))
  testthat::expect_true(all(unlist(lapply(pred, is.finite))))
})
