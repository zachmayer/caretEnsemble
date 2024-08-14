testthat::test_that("LDLcalc package model", {
  # The LDLcalc package has a saved model that's an old caretStack
  # with old train models in it. Some of them make predictions
  # that are vectors, not lists. This is a test to make sure that
  # these old models still work

  test_model <- readRDS(file.path("data", "caretlist_with_bad_earth_model.rds"))
  testthat::expect_s3_class(test_model, "caretList")

  dat <- data.table::data.table(
    AGE = c(25L, 78L, 94L, 60L, 82L, 87L),
    CHOL = c(152L, 134L, 187L, 176L, 141L, 109L),
    TG = c(189L, 101L, 85L, 94L, 114L, 72L),
    HDL = c(41L, 39L, 76L, 69L, 35L, 38L),
    LDLd = c(81L, 83L, 100L, 86L, 94L, 61L)
  )

  pred <- predict(test_model, dat)
})
