# Setup
utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)
utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)

set.seed(1234L)

ens.reg <- caretEnsemble(
  models.reg,
  trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final")
)

ens.class <- caretEnsemble(
  models.class,
  metric = "ROC",
  trControl = caret::trainControl(
    number = 2L,
    summaryFunction = caret::twoClassSummary,
    classProbs = TRUE,
    savePredictions = TRUE
  )
)

mod <- caret::train(
  X.reg,
  Y.reg,
  method = "lm",
  trControl = caret::trainControl(method = "none")
)

# Helper function for testing
expect_data_table_structure <- function(dt, expected_names) {
  testthat::expect_s3_class(dt, "data.table")
  testthat::expect_named(dt, expected_names)
}

#############################################################################
testthat::context("caretPredict and extractMetric")
#############################################################################
testthat::test_that("caretPredict extracts best predictions correctly", {
  stacked_preds_class <- caretPredict(models.class[[1L]], excluded_class_id = 0L)
  stacked_preds_reg <- caretPredict(models.reg[[1L]])

  expect_data_table_structure(stacked_preds_class, c("No", "Yes"))
  expect_data_table_structure(stacked_preds_reg, "pred")
})

testthat::test_that("extractMetric works for different model types", {
  # Test for model with no resampling (no SD)
  metric <- extractMetric(mod)
  expect_data_table_structure(metric, c("model_name", "metric", "value", "sd"))
  testthat::expect_true(is.na(metric$value), is.na(metric$sd))

  # Test for ensemble models
  for (ens in list(ens.class, ens.reg)) {
    metrics <- extractMetric(ens)
    expect_data_table_structure(metrics, c("model_name", "metric", "value", "sd"))
    testthat::expect_named(ens$models, metrics$model_name[-1L])
  }
})

#############################################################################
testthat::context("S3 methods and model operations")
#############################################################################
testthat::test_that("c.train on 2 train objects", {
  testthat::expect_error(c.train(list()), "class of modelList1 must be 'caretList' or 'train'")

  combined_models <- c(models.class[[1L]], models.class[[1L]])
  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, 2L)
  testthat::expect_identical(anyDuplicated(names(combined_models)), 0L)
  testthat::expect_length(unique(names(combined_models)), 2L)
})

testthat::test_that("c.train on a train and a caretList", {
  bigList <- c(models.reg[[1L]], models.class)
  testthat::expect_is(bigList, "caretList")
  testthat::expect_identical(anyDuplicated(names(bigList)), 0L)
  testthat::expect_length(unique(names(bigList)), 5L)
})

#############################################################################
testthat::context("isClassifierAndValidate")
#############################################################################
testthat::test_that("isClassifierAndValidate handles various model types", {
  models_multi <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm")
  )
  models_multi_bin_reg <- c(models_multi, models.class, models.reg)
  testthat::expect_is(vapply(models_multi_bin_reg, isClassifierAndValidate, logical(1L)), "logical")

  # Test when predictions are missing
  model_list <- models.class
  model_list[[1L]]$pred <- NULL
  testthat::expect_is(vapply(model_list, isClassifierAndValidate, logical(1L)), "logical")
  testthat::expect_equivalent(unique(vapply(model_list, isClassifierAndValidate, logical(1L))), TRUE)

  # Test error cases
  model_list <- models.class
  model_list[[1L]]$modelInfo$prob <- FALSE
  testthat::expect_error(
    lapply(model_list, isClassifierAndValidate),
    "No probability function found. Re-fit with a method that supports prob."
  )

  model_list <- models.class
  model_list[[1L]]$control$classProbs <- FALSE
  testthat::expect_error(
    lapply(model_list, isClassifierAndValidate, validate_for_stacking = TRUE),
    "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl."
  )

  # Test for non-caretList object
  testthat::expect_error(
    isClassifierAndValidate(list(model = lm(Y.reg ~ ., data = as.data.frame(X.reg)))),
    "is(object, \"train\") is not TRUE",
    fixed = TRUE
  )

  # Test for models without savePredictions
  model <- models.class[[1L]]
  model$control$savePredictions <- NULL
  testthat::expect_error(
    isClassifierAndValidate(model),
    "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
  )
  model$control$savePredictions <- "BAD_VALUE"
  testthat::expect_error(
    isClassifierAndValidate(model),
    "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
  )
})

#############################################################################
testthat::context("validateExcludedClass")
#############################################################################
testthat::test_that("validateExcludedClass handles various inputs", {
  testthat::expect_error(validateExcludedClass("invalid"), "classification excluded level must be numeric: invalid")
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"),
    "classification excluded level is not an integer: Inf"
  )
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(-1.0), "classification excluded level must be >= 0: -1"),
    "classification excluded level is not an integer:"
  )
  testthat::expect_warning(validateExcludedClass(1.1), "classification excluded level is not an integer: 1.1")
  testthat::expect_identical(validateExcludedClass(3L), 3L)

  # Edge cases
  testthat::expect_identical(validateExcludedClass(0L), 0L)
  testthat::expect_identical(validateExcludedClass(1L), 1L)
  testthat::expect_identical(validateExcludedClass(4L), 4L)
  w <- "classification excluded level is not an integer:"
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(0.0), 0L), w)
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(1.0), 1L), w)
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(4.0), 4L), w)
  testthat::expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")

  # Additional tests
  testthat::expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  testthat::expect_error(
    validateExcludedClass(c(1L, 2L)),
    "classification excluded level must have a length of 1: length=2"
  )
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"),
    "classification excluded level is not an integer"
  )
})
