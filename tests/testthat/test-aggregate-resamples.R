context("Test aggregate_resamples functionality")

test_that("extractBestPreds respects aggregate_resamples parameter", {
  # Create a simple model with repeated CV
  data(iris)
  tc <- caret::trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 2,
    savePredictions = "final"
  )
  
  model <- caret::train(
    Sepal.Length ~ .,
    data = iris,
    method = "lm",
    trControl = tc
  )
  
  # Test with aggregation (default)
  preds_agg <- extractBestPreds(model, aggregate_resamples = TRUE)
  expect_equal(nrow(preds_agg), nrow(iris))
  
  # Test without aggregation
  preds_no_agg <- extractBestPreds(model, aggregate_resamples = FALSE)
  expect_equal(nrow(preds_no_agg), nrow(iris) * 4) # 2 folds * 2 repeats
  expect_true(nrow(preds_no_agg) > nrow(preds_agg))
})

test_that("caretList respects aggregate_resamples parameter", {
  data(iris)
  tc <- caret::trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 2,
    savePredictions = "final"
  )
  
  # Test with aggregation (default)
  model_list_agg <- caretList(
    Sepal.Length ~ .,
    data = iris,
    trControl = tc,
    methodList = c("lm", "rf"),
    tuneLength = 1
  )
  
  # Test without aggregation
  model_list_no_agg <- caretList(
    Sepal.Length ~ .,
    data = iris,
    trControl = tc,
    methodList = c("lm", "rf"),
    tuneLength = 1,
    aggregate_resamples = FALSE
  )
  
  # Check predictions from both models
  expect_equal(nrow(model_list_agg$lm$pred), nrow(iris))
  expect_equal(nrow(model_list_no_agg$lm$pred), nrow(iris) * 4) # 2 folds * 2 repeats
  expect_true(nrow(model_list_no_agg$lm$pred) > nrow(model_list_agg$lm$pred))
})
