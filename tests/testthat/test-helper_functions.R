suppressMessages({
  library(testthat)
  library(caret)
  library(rpart)
})

########################################################################
context("Do the helper functions work for regression objects?")
########################################################################

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

test_that("Recycling generates a warning", {
  expect_error(caretEnsemble::wtd.sd(matrix(1L:10L, ncol = 2L), w = 1L))
})

test_that("No predictions generates an error", {
  suppressWarnings(
    models_multi <- caretList(
      iris[, 1L:2L], iris[, 5L],
      tuneLength = 1L, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
    )
  )
  check_caretList_model_types(models_multi)

  suppressWarnings(
    models <- caretList(
      iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
      tuneLength = 1L, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
    )
  )
  new_model <- train(
    iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
    tuneLength = 1L,
    method = "glmnet",
    trControl = trainControl(method = "cv", number = 2L, savePredictions = "none", classProbs = TRUE)
  )
  models2 <- c(new_model, models)
  models3 <- c(models, new_model)
  check_caretList_model_types(models)
  expect_error(check_caretList_model_types(models2))
  expect_error(check_caretList_model_types(models3))
})

test_that("We can make the predobs matrix", {
  out <- extractBestPredsAndObs(models.reg)
  expect_is(out, "list")
  expect_length(out$obs, 150L)
  expect_identical(dim(out$preds), c(150L, 4L))
})

test_that("We can predict", {
  out <- predict(models.reg, newdata = X.reg)
  expect_is(out, "data.table")
  expect_identical(dim(out), c(150L, 4L))
  expect_named(out, c("rf", "glm", "rpart", "treebag"))
})

########################################################################
context("Do the helper functions work for classification objects?")
########################################################################

test_that("We can make the predobs matrix", {
  out <- extractBestPredsAndObs(models.class)
  expect_that(out, is_a("list"))
  expect_length(out$obs, 150L)
  expect_identical(dim(out$preds), c(150L, 4L * 1L)) # number of models * (number of classes-1)
})

test_that("We can predict", {
  out <- predict(models.class, newdata = X.class)
  expect_is(out, "data.table")
  expect_identical(dim(out), c(150L, 4L * 2L))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  expect_true(all(colnames(out) == paste(combinations$Var2, combinations$Var1, sep = "_")))
  out2 <- predict(models.reg, newdata = X.reg)
  expect_identical(dim(out2), c(150L, 4L))
  expect_true(all(colnames(out2) == c("rf", "glm", "rpart", "treebag")))
})

test_that("predict results same regardless of verbose option", {
  invisible(capture.output({
    suppressWarnings({
      expect_is(predict(models.class, newdata = X.class), "data.table")
      out1 <- predict(models.class, newdata = X.class)
      out2 <- predict(models.class, verbose = TRUE, newdata = X.class)
      expect_identical(out1, out2)
    })

    expect_is(predict(models.reg, newdata = X.reg), "data.table")
    out1 <- predict(models.reg, newdata = X.reg)
    out2 <- predict(models.reg, verbose = TRUE, newdata = X.reg)
    expect_identical(out1, out2)
  }))
})

context("Test weighted standard deviations")

test_that("wtd.sd applies weights correctly", {
  x1 <- c(3L, 5L, 9L, 3L, 4L, 6L, 4L)
  x2 <- c(10L, 10L, 20L, 14L, 2L, 2L, 40L)
  x3 <- c(10L, 10L, 10L, 20L)
  w1 <- c(0.1, 0.1, 0.1, 0.7)
  expect_error(caretEnsemble::wtd.sd(x1), 'argument "w" is missing, with no default')
  expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  expect_equal(caretEnsemble::wtd.sd(x3, w = w1), 5.291503, tolerance = 0.001)
  expect_equal(caretEnsemble::wtd.sd(x3, w = w1 * 100L), caretEnsemble::wtd.sd(x3, w = w1))
})

test_that("wtd.sd handles NA values correctly", {
  x1 <- c(10L, 10L, 10L, 20L, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  expect_true(is.na(sd(x1)))
  expect_false(is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
  expect_false(is.na(sd(x1, na.rm = TRUE)))
  expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  expect_false(is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
})

test_that("Checks generate errors", {
  set.seed(42L)
  myControl <- trainControl(method = "cv", number = 5L, savePredictions = "final")
  expect_warning(
    x <- caretList(
      Sepal.Length ~ Sepal.Width,
      iris,
      methodList = c("glm", "lm"),
      trControl = myControl
    )
  )
  modelLibrary <- extractBestPredsAndObs(x)
  idx <- sample(seq_along(modelLibrary$preds$lm), length(modelLibrary$preds$lm))
  modelLibrary$preds$nn <- modelLibrary$preds$lm[idx]

  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_warning(x$rpart <- train(Sepal.Length ~ Sepal.Width, iris, method = "rpart"))
  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_error(check_caretList_classes(x$glm$finalModel))

  x$rpart <- train(Species ~ Sepal.Width, iris, method = "rpart", trControl = myControl)
  check_caretList_classes(x)
  expect_error(check_caretList_model_types(x))

  expect_error(m <- extractBestPredsAndObs(x))

  set.seed(42L)
  myControl2 <- trainControl(
    method = "cv",
    number = 10L,
    savePredictions = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  suppressWarnings(
    x <- caretList(
      iris[1L:100L, -5L],
      factor(ifelse(iris[1L:100L, "Species"] == "setosa", "Yes", "No")),
      methodList = c("lda", "rf"),
      trControl = myControl2
    )
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, iris, method = "rpart")
  expect_error(check_caretList_model_types(x))
})

test_that("check_caretList_model_types stops when there are no predictions saved", {
  model_list <- models.class
  model_list[[1L]]$pred <- NULL
  err <- "No predictions saved by train. Please re-run models with trainControl savePredictions = 'final'"
  expect_error(check_caretList_model_types(model_list), err)
})

test_that("check_caretList_model_types stops when a classification model support probabilities", {
  model_list <- models.class
  model_list[[1L]]$modelInfo$prob <- FALSE
  err <- "All models for classification must be able to generate class probabilities."
  expect_error(check_caretList_model_types(model_list), err)
})

test_that("check_caretList_model_types stops when a classification model did not save probs", {
  model_list <- models.class
  model_list[[1L]]$control$classProbs <- FALSE
  m <- "No probability function found.  Re-fit with a method that supports prob."
  expect_error(lapply(model_list, extractModelType), m)
  context("Test helper functions for multiclass classification")

  test_that("Check errors in caretEnsemble for multiclass classification work", {
    skip_on_cran()
    data(iris)
    myControl <- trainControl(
      method = "cv",
      number = 5L,
      savePredictions = "final",
      index = createResample(iris[, 5L], 5L)
    )
    model_list <- caretList(
      x = iris[, -5L],
      y = iris[, 5L],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    expect_error(check_binary_classification(model_list))
    expect_null(check_binary_classification(models.class))
    expect_null(check_binary_classification(models.reg))

    # Do not produce errors when another object is passed
    expect_null(check_binary_classification(NULL))
    expect_null(check_binary_classification(2L))
    expect_null(check_binary_classification(list("string")))
    expect_null(check_binary_classification(iris))
  })

  test_that("Configuration function for excluded level work", {
    # Integers work
    expect_equal(validateExcludedClass(0L), 0L)
    expect_equal(validateExcludedClass(1L), 1L)
    expect_equal(validateExcludedClass(4L), 4L)

    # Decimals work with a warning
    expect_warning(expect_equal(validateExcludedClass(0.0), 0L))
    expect_warning(expect_equal(validateExcludedClass(1.0), 1L))
    expect_warning(expect_equal(validateExcludedClass(4.0), 4L))

    # Less than 0 will error
    expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")

    # Make a model list
    data(iris)
    myControl <- trainControl(
      method = "cv", number = 5L,
      savePredictions = "final",
      index = createResample(iris[, 5L], 5L),
      classProbs = TRUE
    )
    model_list <- caretList(
      x = iris[, -5L],
      y = iris[, 5L],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    # Stacking with the excluded level should work
    invisible(caretStack(model_list, method = "knn", excluded_class_id = 1L))

    # Stacking with too great of a level should work.  No error or warning.
    # TODO: maybe caretStack should raise a warning if excluded_class_id is too high?
    # Should also validate it?
    stack <- caretStack(model_list, method = "knn", excluded_class_id = 4L)
    invisible(predict(stack, iris[, -5L]))

    # Check if we are actually excluding level 1 (setosa)
    classes <- levels(iris[, 5L])[-1L]
    models <- c("rpart", "glmnet")
    class_model_combinations <- expand.grid(classes, models)
    varImp_rownames <- apply(class_model_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))

    model_stack <- caretStack(model_list, method = "knn", excluded_class_id = 1L)
    expect_identical(rownames(varImp(model_stack$ens_model)$importance), varImp_rownames)
  })
})

# Tests for validateExcludedClass function
test_that("validateExcludedClass stops for non-numeric input", {
  invalid_input <- "invalid"
  err <- "classification excluded level must be numeric: invalid"
  expect_error(validateExcludedClass(invalid_input), err)
})

test_that("validateExcludedClass stops for non-finite input", {
  invalid_input <- Inf
  err <- "classification excluded level must be finite: Inf"
  expect_warning(
    expect_error(validateExcludedClass(invalid_input), err),
    "classification excluded level is not an integer: Inf"
  )
})

test_that("validateExcludedClass stops for non-positive input", {
  invalid_input <- -1.0
  err <- "classification excluded level must be >= 0: -1"
  expect_warning(expect_error(validateExcludedClass(invalid_input), err))
})

test_that("validateExcludedClass warns for non-integer input", {
  expect_warning(
    validated <- validateExcludedClass(1.1),
    "classification excluded level is not an integer: 1.1"
  )
  expect_equal(validated, 1L)
})

test_that("validateExcludedClass passes for valid input", {
  valid_input <- 3L
  expect_equal(validateExcludedClass(valid_input), 3L)
})

########################################################################
context("Helper function edge cases")
########################################################################

test_that("wtd.sd calculates weighted standard deviation correctly", {
  x <- c(1L, 2L, 3L, 4L, 5L)
  w <- c(1L, 1L, 1L, 1L, 1L)
  expect_equal(wtd.sd(x, w), sd(x))

  w <- c(2L, 1L, 1L, 1L, 1L)
  expect_true(wtd.sd(x, w) != sd(x))

  # Test with NA values
  x_na <- c(1L, 2L, NA, 4L, 5L)
  expect_true(is.na(wtd.sd(x_na, w)))
  expect_false(is.na(wtd.sd(x_na, w, na.rm = TRUE)))

  # Test error for mismatched lengths
  expect_error(wtd.sd(x, w[-1L]))
})

test_that("check_caretList_classes validates caretList correctly", {
  expect_null(check_caretList_classes(models.class))
  expect_null(check_caretList_classes(models.reg))

  # Test error for non-caretList object
  expect_error(check_caretList_classes(list(model = lm(Y.reg ~ ., data = as.data.frame(X.reg)))))
})

test_that("check_caretList_model_types validates model types correctly", {
  expect_null(check_caretList_model_types(models.class))
  expect_null(check_caretList_model_types(models.reg))

  # Test error for mixed model types
  mixed_list <- c(models.class, models.reg[1L])
  class(mixed_list) <- "caretList"
  expect_error(check_caretList_model_types(mixed_list))
})

test_that("extractBestPredsAndObs works", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_is(best_preds_class, "list")
  expect_is(best_preds_reg, "list")

  expect_named(best_preds_class$preds, names(models.class))
  expect_named(best_preds_reg$preds, names(models.reg))
})

test_that("extractBestPredsAndObs errors on inconsistent resamples", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$Resample <- "BAD_SAMPLE"
  err <- "Component models do not have the same re-sampling strategies"
  expect_error(extractBestPredsAndObs(models.class.inconsistent), err)
})

test_that("extractBestPredsAndObs errors on inconsistent row indexes", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$rowIndex <- 0L
  err <- "Re-sampled predictions from each component model do not use the same rowIndexes from the origial dataset"
  expect_error(extractBestPredsAndObs(models.class.inconsistent), err)
})

test_that("extractBestPredsAndObs errors on inconsistent obs", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$obs <- -Inf
  err <- "Observed values for each component model are not the same. Re-train the models with the same Y variable"
  expect_error(extractBestPredsAndObs(models.class.inconsistent), err)
})

test_that("extractBestPredsAndObs errors on inconsistent pred", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$pred <- "BAD_PRED"
  err <- "Component models do not all have the same type of predicitons.  Predictions are a mix of character, factor"
  expect_error(extractBestPredsAndObs(models.class.inconsistent), err)
})

test_that("extractModelName extracts model names correctly", {
  expect_equal(extractModelName(models.class[[1L]]), "rf")
  expect_equal(extractModelName(models.reg[[1L]]), "rf")

  # Test custom model
  custom_model <- models.class[[1L]]
  custom_model$method <- list(method = "custom_rf")
  expect_equal(extractModelName(custom_model), "custom_rf")
})

test_that("extractModelType extracts model types correctly", {
  expect_equal(extractModelType(models.class), "Classification")
  expect_equal(extractModelType(models.reg), "Regression")
})

test_that("extractBestPreds extracts best predictions correctly", {
  best_preds_class <- extractBestPreds(models.class[[1L]])
  best_preds_reg <- extractBestPreds(models.reg[[1L]])

  expect_s3_class(best_preds_class, "data.frame")
  expect_s3_class(best_preds_reg, "data.frame")
  expect_true(all(c("Resample", "rowIndex", "pred", "obs") %in% names(best_preds_class)))
  expect_true(all(c("Resample", "rowIndex", "pred", "obs") %in% names(best_preds_reg)))
})

test_that("extractBestPredsAndObs extracts best predictions for all models", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_type(best_preds_class, "list")
  expect_type(best_preds_reg, "list")

  expect_equal(ncol(best_preds_class$preds), length(models.class))
  expect_equal(ncol(best_preds_reg$preds), length(models.reg))

  expect_named(best_preds_class$pred, names(models.class))
  expect_named(best_preds_reg$pred, names(models.reg))

  expected_names <- c("preds", "obs", "rowIndex", "Resample", "type")
  expect_named(best_preds_class, expected_names)
  expect_named(best_preds_reg, expected_names)
})

test_that("extractBestPredsAndObs creates prediction-observation data correctly", {
  pred_obs_matrix_class <- extractBestPredsAndObs(models.class)
  pred_obs_matrix_reg <- extractBestPredsAndObs(models.reg)

  expect_type(pred_obs_matrix_class, "list")
  expect_type(pred_obs_matrix_reg, "list")
  expect_true(all(c("obs", "preds", "type") %in% names(pred_obs_matrix_class)))
  expect_true(all(c("obs", "preds", "type") %in% names(pred_obs_matrix_reg)))
  expect_equal(nrow(pred_obs_matrix_class$preds), length(pred_obs_matrix_class$obs))
  expect_equal(nrow(pred_obs_matrix_reg$preds), length(pred_obs_matrix_reg$obs))
})

test_that("extractBestPredsAndObs fails on new model types", {
  models.class.new <- models.class
  for (idx in seq_along(models.class.new)) {
    models.class.new[[idx]]$modelType <- "ObjectDetection"
  }
  expect_error(extractBestPredsAndObs(models.class.new), "Unknown model type: ObjectDetection")
})

test_that("validateExcludedClass validates excluded level correctly", {
  expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  expect_error(validateExcludedClass(c(1L, 2L)), "classification excluded level must have a length of 1: length=2")
  expect_error(validateExcludedClass("a"), "classification excluded level must be numeric: a")
  expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
  expect_warning(expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"))
  expect_warning(expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"))
  expect_warning(validateExcludedClass(1.5), "classification excluded level is not an integer: 1.5")
  txt <- "classification excluded level is not an integer: 2"
  expect_warning(expect_equal(validateExcludedClass(2.0), 2L), txt)
})
