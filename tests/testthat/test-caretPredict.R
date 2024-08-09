data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

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

#############################################################################
testthat::context("caretPredict")
#############################################################################
testthat::test_that("caretPredict extracts best predictions correctly", {
  stacked_preds_class <- caretPredict(models.class[[1L]], excluded_class_id = 0L)
  stacked_preds_reg <- caretPredict(models.reg[[1L]])

  testthat::expect_s3_class(stacked_preds_class, "data.table")
  testthat::expect_s3_class(stacked_preds_reg, "data.table")

  testthat::expect_named(stacked_preds_class, c("No", "Yes"))
  testthat::expect_named(stacked_preds_reg, "pred")
})

#############################################################################
testthat::context("extractMetric")
#############################################################################
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

testthat::test_that("extractMetric", {
  for (ens in list(ens.class, ens.reg)) {
    metrics <- extractMetric(ens)
    testthat::expect_s3_class(metrics, "data.table")
    testthat::expect_named(ens$models, metrics$model_name[-1L])
  }
})

#############################################################################
testthat::context("extractModelName")
#############################################################################

testthat::test_that("extractModelName handles custom models correctly", {
  mock_model <- list(method = list(method = "custom_method"))
  class(mock_model) <- "train"
  testthat::expect_identical(extractModelName(mock_model), "custom_method")
})

testthat::test_that("extractModelName handles custom models correctly", {
  mock_model <- list(method = "custom", modelInfo = list(method = "custom_method"))
  class(mock_model) <- "train"
  testthat::expect_identical(extractModelName(mock_model), "custom_method")
})

testthat::test_that("extractModelName extracts model names correctly", {
  testthat::expect_identical(extractModelName(models.class[[1L]]), "rf")
  testthat::expect_identical(extractModelName(models.reg[[1L]]), "rf")

  # Test custom model
  custom_model <- models.class[[1L]]
  custom_model$method <- list(method = "custom_rf")
  testthat::expect_identical(extractModelName(custom_model), "custom_rf")
})

#############################################################################
testthat::context("isClassifierAndValidate")
#############################################################################

testthat::test_that("isClassifierAndValidate", {
  models_multi <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm")
  )
  models_multi_bin_reg <- c(models_multi, models.class, models.reg)
  testthat::expect_is(vapply(models_multi_bin_reg, isClassifierAndValidate, logical(1L)), "logical")
})

testthat::test_that("isClassifierAndValidate shouldn't care about predictions", {
  model_list <- models.class
  model_list[[1L]]$pred <- NULL
  testthat::expect_is(vapply(model_list, isClassifierAndValidate, logical(1L)), "logical")
  testthat::expect_equivalent(unique(vapply(model_list, isClassifierAndValidate, logical(1L))), TRUE)
})

testthat::test_that("isClassifierAndValidate stops when a classification model can't predict probabilities", {
  model_list <- models.class
  model_list[[1L]]$modelInfo$prob <- FALSE
  err <- "No probability function found. Re-fit with a method that supports prob."
  testthat::expect_error(lapply(model_list, isClassifierAndValidate), err)
})

testthat::test_that("isClassifierAndValidate stops when a classification model did not save probs", {
  model_list <- models.class
  model_list[[1L]]$control$classProbs <- FALSE
  err <- "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl."
  testthat::expect_error(lapply(model_list, isClassifierAndValidate, validate_for_stacking = TRUE), err)
  testthat::context("Test helper functions for multiclass classification")
})

testthat::test_that("isClassifierAndValidate validates caretList correctly", {
  testthat::expect_is(vapply(models.class, isClassifierAndValidate, logical(1L)), "logical")
  testthat::expect_is(vapply(models.reg, isClassifierAndValidate, logical(1L)), "logical")

  # Test error for non-caretList object
  testthat::expect_error(
    isClassifierAndValidate(list(model = lm(Y.reg ~ ., data = as.data.frame(X.reg)))),
    "is(object, \"train\") is not TRUE",
    fixed = TRUE
  )
})

testthat::test_that("isClassifierAndValidate validates model types correctly", {
  testthat::expect_is(vapply(models.class, isClassifierAndValidate, logical(1L)), "logical")
  testthat::expect_is(vapply(models.reg, isClassifierAndValidate, logical(1L)), "logical")

  # Test error for mixed model types
  mixed_list <- c(models.class, models.reg)
  testthat::expect_is(vapply(mixed_list, isClassifierAndValidate, logical(1L)), "logical")
})

testthat::test_that("isClassifierAndValidate extracts model types correctly", {
  testthat::expect_true(unique(vapply(models.class, isClassifierAndValidate, logical(1L))))
  testthat::expect_false(unique(vapply(models.reg, isClassifierAndValidate, logical(1L))))
})

testthat::test_that("isClassifierAndValidate fails for models without object$control$savePredictions", {
  model <- models.class[[1L]]
  model$control$savePredictions <- NULL
  err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
  testthat::expect_error(isClassifierAndValidate(model), err)
  model$control$savePredictions <- "BAD_VALUE"
  testthat::expect_error(isClassifierAndValidate(model), err)
})

#############################################################################
testthat::context("validateExcludedClass")
#############################################################################

testthat::test_that("validateExcludedClass stops for non-numeric input", {
  invalid_input <- "invalid"
  err <- "classification excluded level must be numeric: invalid"
  testthat::expect_error(validateExcludedClass(invalid_input), err)
})

testthat::test_that("validateExcludedClass stops for non-finite input", {
  invalid_input <- Inf
  err <- "classification excluded level must be finite: Inf"
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(invalid_input), err),
    "classification excluded level is not an integer: Inf"
  )
})

testthat::test_that("validateExcludedClass stops for non-positive input", {
  invalid_input <- -1.0
  err <- "classification excluded level must be >= 0: -1"
  wrn <- "classification excluded level is not an integer:"
  testthat::expect_warning(testthat::expect_error(validateExcludedClass(invalid_input), err), wrn)
})

validated <- testthat::test_that("validateExcludedClass warns for non-integer input", {
  testthat::expect_identical(
    testthat::expect_warning(
      validateExcludedClass(1.1),
      "classification excluded level is not an integer: 1.1"
    ), 1L
  )
})

testthat::test_that("validateExcludedClass passes for valid input", {
  valid_input <- 3L
  testthat::expect_identical(validateExcludedClass(valid_input), 3L)
})

testthat::test_that("validateExcludedClass edge cases", {
  # Integers work
  testthat::expect_identical(validateExcludedClass(0L), 0L)
  testthat::expect_identical(validateExcludedClass(1L), 1L)
  testthat::expect_identical(validateExcludedClass(4L), 4L)

  # Decimals work with a warning
  wrn <- "classification excluded level is not an integer:"
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(0.0), 0L), wrn)
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(1.0), 1L), wrn)
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(4.0), 4L), wrn)

  # Less than 0 will error
  testthat::expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
})

testthat::test_that("validateExcludedClass works with caretLists", {
  # Make a model list
  data(iris)
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    methodList = c("rpart", "glmnet")
  )

  # Stacking with the excluded level should work
  invisible(caretStack(model_list, method = "knn", excluded_class_id = 1L))

  # Stacking with too great of a level should work. No error or warning.
  stack <- caretStack(model_list, method = "knn", excluded_class_id = 4L)
  invisible(predict(stack, iris[, -5L]))

  # Check if we are actually excluding level 1 (setosa)
  classes <- levels(iris[, 5L])[-1L]
  models <- c("rpart", "glmnet")
  class_model_combinations <- expand.grid(classes, models)
  varImp_rownames <- apply(class_model_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))

  model_stack <- caretStack(model_list, method = "knn", excluded_class_id = 1L)
  testthat::expect_identical(rownames(caret::varImp(model_stack$ens_model)$importance), varImp_rownames)
})

testthat::test_that("validateExcludedClass validates excluded level correctly", {
  testthat::expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  testthat::expect_error(
    validateExcludedClass(c(1L, 2L)),
    "classification excluded level must have a length of 1: length=2"
  )
  testthat::expect_error(validateExcludedClass("a"), "classification excluded level must be numeric: a")
  testthat::expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"),
    "classification excluded level is not an integer"
  )
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"),
    "classification excluded level is not an integer"
  )
  testthat::expect_warning(validateExcludedClass(1.5), "classification excluded level is not an integer: 1.5")
  txt <- "classification excluded level is not an integer: 2"
  testthat::expect_warning(
    testthat::expect_identical(
      validateExcludedClass(2.0), 2L
    ), txt,
    "classification excluded level is not an integer"
  )
})

testthat::test_that("validateExcludedClass validates excluded level correctly", {
  testthat::expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  testthat::expect_error(
    validateExcludedClass(c(1L, 2L)),
    "classification excluded level must have a length of 1: length=2"
  )
  testthat::expect_error(validateExcludedClass("a"), "classification excluded level must be numeric: a")
  testthat::expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"),
    "classification excluded level is not an integer"
  )
  testthat::expect_warning(
    testthat::expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"),
    "classification excluded level is not an integer"
  )
  testthat::expect_warning(validateExcludedClass(1.5), "classification excluded level is not an integer: 1.5")
  txt <- "classification excluded level is not an integer: 2"
  testthat::expect_warning(testthat::expect_identical(validateExcludedClass(2.0), 2L), txt)
})
