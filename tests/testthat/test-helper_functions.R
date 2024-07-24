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
  expect_error(caretEnsemble::wtd.sd(matrix(1:10, ncol = 2), w = 1))
})

test_that("No predictions generates an error", {
  suppressWarnings(
    models_multi <- caretList(
      iris[, 1:2], iris[, 5],
      tuneLength = 1, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
    )
  )
  check_caretList_model_types(models_multi)

  suppressWarnings(
    models <- caretList(
      iris[, 1:2], factor(ifelse(iris[, 5] == "setosa", "Yes", "No")),
      tuneLength = 1, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
    )
  )
  new_model <- train(
    iris[, 1:2], factor(ifelse(iris[, 5] == "setosa", "Yes", "No")),
    tuneLength = 1,
    method = "glmnet",
    trControl = trainControl(method = "cv", number = 2, savePredictions = "none", classProbs = TRUE)
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
  expect_true(length(out$obs) == 150)
  expect_true(all(dim(out$preds) == c(150, 4)))
})

test_that("We can predict", {
  out <- predict(models.reg, newdata = X.reg)
  expect_is(out, "data.table")
  expect_true(all(dim(out) == c(150, 4)))
  expect_true(all(colnames(out) == c("rf", "glm", "rpart", "treebag")))
})

########################################################################
context("Do the helper functions work for classification objects?")
########################################################################

test_that("We can make the predobs matrix", {
  out <- extractBestPredsAndObs(models.class)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs) == 150)
  expect_true(all(dim(out$preds) == c(150, 4 * 1))) # number of models * (number of classes-1)
})

test_that("We can predict", {
  out <- predict(models.class, newdata = X.class)
  expect_is(out, "data.table")
  expect_true(all(dim(out) == c(150, 4 * 2)))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  expect_true(all(colnames(out) == paste(combinations$Var2, combinations$Var1, sep = ".")))
  out2 <- predict(models.reg, newdata = X.reg)
  expect_true(all(dim(out2) == c(150, 4)))
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
  x1 <- c(3, 5, 9, 3, 4, 6, 4)
  x2 <- c(10, 10, 20, 14, 2, 2, 40)
  x3 <- c(10, 10, 10, 20)
  w1 <- c(0.1, 0.1, 0.1, 0.7)
  expect_error(caretEnsemble::wtd.sd(x1), 'argument "w" is missing, with no default')
  expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  expect_equal(caretEnsemble::wtd.sd(x3, w = w1), 5.291503, tolerance = .001)
  expect_equal(caretEnsemble::wtd.sd(x3, w = w1 * 100), caretEnsemble::wtd.sd(x3, w = w1))
})

test_that("wtd.sd handles NA values correctly", {
  x1 <- c(10, 10, 10, 20, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  expect_true(is.na(sd(x1)))
  expect_true(!is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
  expect_true(!is.na(sd(x1, na.rm = TRUE)))
  expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  expect_true(!is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
})

test_that("Checks generate errors", {
  set.seed(42)
  myControl <- trainControl(method = "cv", number = 5, savePredictions = "final")
  expect_warning(
    x <- caretList(
      Sepal.Length ~ Sepal.Width,
      iris,
      methodList = c("glm", "lm"),
      trControl = myControl
    )
  )
  modelLibrary <- extractBestPredsAndObs(x)
  modelLibrary$preds$nn <- modelLibrary$preds$lm[sample(seq_along(modelLibrary$preds$lm), length(modelLibrary$preds$lm))]

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

  set.seed(42)
  myControl2 <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  suppressWarnings(
    x <- caretList(
      iris[1:100, -5],
      factor(ifelse(iris[1:100, "Species"] == "setosa", "Yes", "No")),
      methodList = c("lda", "rf"),
      trControl = myControl2
    )
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, iris, method = "rpart")
  expect_error(check_caretList_model_types(x))
})

test_that("check_caretList_model_types stops when there are no predictions saved", {
  model_list <- models.class
  model_list[[1]]$pred <- NULL
  expect_error(check_caretList_model_types(model_list), "No predictions saved by train. Please re-run models with trainControl set with savePredictions = 'final'.")
})

test_that("check_caretList_model_types stops when a classification model support probabilities", {
  model_list <- models.class
  model_list[[1]]$modelInfo$prob <- FALSE
  expect_error(check_caretList_model_types(model_list), "All models for classification must be able to generate class probabilities.")
})

test_that("check_caretList_model_types stops when a classification model supports probabilities but did not save them", {
  model_list <- models.class
  model_list[[1]]$control$classProbs <- FALSE
  m <- "Some models were fit with no class probabilities. Please re-fit them with trainControl, classProbs = TRUE: rf"
  expect_error(check_caretList_model_types(model_list), m)
  context("Test helper functions for multiclass classification")

  test_that("Check errors in caretEnsemble for multiclass classification work", {
    skip_on_cran()
    data(iris)
    myControl <- trainControl(method = "cv", number = 5, savePredictions = "final", index = createResample(iris[, 5], 5))
    model_list <- caretList(
      x = iris[, -5],
      y = iris[, 5],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    expect_error(check_binary_classification(model_list))
    expect_true(is.null(check_binary_classification(models.class)))
    expect_true(is.null(check_binary_classification(models.reg)))

    # Do not produce errors when another object is passed
    expect_true(is.null(check_binary_classification(NULL)))
    expect_true(is.null(check_binary_classification(2)))
    expect_true(is.null(check_binary_classification(list("string"))))
    expect_true(is.null(check_binary_classification(iris)))
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
      method = "cv", number = 5,
      savePredictions = "final",
      index = createResample(iris[, 5], 5),
      classProbs = TRUE
    )
    model_list <- caretList(
      x = iris[, -5],
      y = iris[, 5],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    # Stacking with the excluded level should work
    invisible(caretStack(model_list, method = "knn", excluded_class_id = 1L))

    # Stacking with too great of a level should work.  No error or warning.
    # TODO: maybe caretStack should raise a warning if excluded_class_id is too high?
    # Should also validate it?
    stack <- caretStack(model_list, method = "knn", excluded_class_id = 4L)
    invisible(predict(stack, iris[, -5]))

    # Check if we are actually excluding level 1 (setosa)
    classes <- levels(iris[, 5])[-1]
    models <- c("rpart", "glmnet")
    class_model_combinations <- expand.grid(classes, models)
    varImp_rownames <- apply(class_model_combinations, 1, function(x) paste(x[2], x[1], sep = "."))

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
  invalid_input <- -1
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
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 1, 1, 1, 1)
  expect_equal(wtd.sd(x, w), sd(x))

  w <- c(2, 1, 1, 1, 1)
  expect_true(wtd.sd(x, w) != sd(x))

  # Test with NA values
  x_na <- c(1, 2, NA, 4, 5)
  expect_true(is.na(wtd.sd(x_na, w)))
  expect_false(is.na(wtd.sd(x_na, w, na.rm = TRUE)))

  # Test error for mismatched lengths
  expect_error(wtd.sd(x, w[-1]))
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
  mixed_list <- c(models.class, models.reg[1])
  class(mixed_list) <- "caretList"
  expect_error(check_caretList_model_types(mixed_list))
})

test_that("check_bestpreds_resamples validates resamples correctly", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_null(check_bestpreds_resamples(best_preds_class))
  expect_null(check_bestpreds_resamples(best_preds_reg))

  # Test error for inconsistent resamples
  best_preds_inconsistent <- best_preds_class
  best_preds_inconsistent[[1]]$Resample <- sample(best_preds_inconsistent[[1]]$Resample)
  expect_error(check_bestpreds_resamples(best_preds_inconsistent))
})

test_that("check_bestpreds_indexes validates row indexes correctly", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_null(check_bestpreds_indexes(best_preds_class))
  expect_null(check_bestpreds_indexes(best_preds_reg))

  # Test error for inconsistent row indexes
  best_preds_inconsistent <- best_preds_class
  best_preds_inconsistent[[1]]$rowIndex <- sample(best_preds_inconsistent[[1]]$rowIndex)
  expect_error(check_bestpreds_indexes(best_preds_inconsistent))
})

test_that("check_bestpreds_obs validates observed values correctly", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_null(check_bestpreds_obs(best_preds_class))
  expect_null(check_bestpreds_obs(best_preds_reg))

  # Test error for inconsistent observed values
  best_preds_inconsistent <- best_preds_class
  best_preds_inconsistent[[1]]$obs <- sample(best_preds_inconsistent[[1]]$obs)
  expect_error(check_bestpreds_obs(best_preds_inconsistent))
})

test_that("check_bestpreds_preds validates predictions correctly", {
  best_preds_class <- extractBestPredsAndObs(models.class)
  best_preds_reg <- extractBestPredsAndObs(models.reg)

  expect_null(check_bestpreds_preds(best_preds_class))
  expect_null(check_bestpreds_preds(best_preds_reg))

  # Test error for inconsistent prediction types
  best_preds_inconsistent <- best_preds_class
  best_preds_inconsistent[[1]]$pred <- as.character(best_preds_inconsistent[[1]]$pred)
  expect_error(check_bestpreds_preds(best_preds_inconsistent))
})

test_that("extractModelName extracts model names correctly", {
  expect_equal(extractModelName(models.class[[1]]), "rf")
  expect_equal(extractModelName(models.reg[[1]]), "rf")

  # Test custom model
  custom_model <- models.class[[1]]
  custom_model$method <- list(method = "custom_rf")
  expect_equal(extractModelName(custom_model), "custom_rf")
})

test_that("extractModelType extracts model types correctly", {
  expect_equal(extractModelType(models.class), "Classification")
  expect_equal(extractModelType(models.reg), "Regression")
})

test_that("extractBestPreds extracts best predictions correctly", {
  best_preds_class <- extractBestPreds(models.class[[1]])
  best_preds_reg <- extractBestPreds(models.reg[[1]])

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

  expect_equal(names(best_preds_class$pred), names(models.class))
  expect_equal(names(best_preds_reg$pred), names(models.reg))

  expected_names <- c("preds", "obs", "rowIndex", "Resample", "type")
  expect_equal(names(best_preds_class), expected_names)
  expect_equal(names(best_preds_reg), expected_names)
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

test_that("validateExcludedClass validates excluded level correctly", {
  expect_error(validateExcludedClass("a"))
  expect_warning(expect_error(validateExcludedClass(-1)))
  expect_warning(expect_error(validateExcludedClass(-0.000001)))
  expect_warning(expect_error(validateExcludedClass(Inf)))
  expect_warning(validateExcludedClass(1.5))
  txt <- "classification excluded level is not an integer: 2"
  expect_warning(expect_equal(validateExcludedClass(2), 2L), txt)
})
