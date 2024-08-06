data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

mod <- caret::train(
  X.reg,
  Y.reg,
  method = "lm",
  trControl = caret::trainControl(method = "none")
)

testthat::test_that("Extracting metrics works if there is no SD", {
  # In the case of no resampling, metrics will not have an SD to extract
  metric <- extractMetric(mod)
  expect_s3_class(metric, "data.table")
  expect_named(metric, c("model_name", "metric", "value", "sd"))
  expect_is(metric$model_name, "character")
  expect_is(metric$metric, "character")
  expect_is(metric$value, "numeric")
  expect_is(metric$sd, "numeric")
  testthat::expect_true(is.na(metric$value))
  testthat::expect_true(is.na(metric$sd))
})
