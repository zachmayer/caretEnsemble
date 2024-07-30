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
  expect_error(caretEnsemble::wtd.sd(matrix(1L:10L, ncol = 2L), w = 1L), "'x' and 'w' must have the same length")
})

test_that("No predictions generates an error", {
  models_multi <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm"),
    trControl = trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
  )
  expect_is(sapply(models_multi, extractModelType), "character")

  models <- caretList(
    iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm"),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
  )
  new_model <- train(
    iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
    tuneLength = 1L,
    method = "glmnet",
    trControl = trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
  )
  models2 <- c(new_model, models)
  models3 <- c(models, new_model)
  expect_is(sapply(models, extractModelType), "character")
  expect_is(sapply(models2, extractModelType), "character")
  expect_is(sapply(models3, extractModelType), "character")
})

test_that("We can make the stacked predictions matrix", {
  out <- predict(models.reg)
  expect_s3_class(out, "data.table")
  expect_identical(dim(out), c(150L, 4L))
  expect_named(out, c("rf", "glm", "rpart", "treebag"))
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

test_that("We can make the stacked predictions matrix", {
  out <- predict(models.class)
  expect_s3_class(out, "data.table")
  expect_identical(dim(out), c(150L, 4L * 1L)) # number of models * (number of classes-1)
})

test_that("We can predict", {
  out <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  expect_is(out, "data.table")
  expect_identical(dim(out), c(150L, 4L * 2L))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  expect_named(out, paste(combinations$Var2, combinations$Var1, sep = "_"))
  out2 <- predict(models.reg, newdata = X.reg)
  expect_identical(dim(out2), c(150L, 4L))
  expect_named(out2, c("rf", "glm", "rpart", "treebag"))
})

test_that("predict results same regardless of verbose option", {
  invisible(capture.output({
    expect_is(predict(models.class, newdata = X.class), "data.table")
    out1 <- predict(models.class, newdata = X.class)
    out2 <- predict(models.class, verbose = TRUE, newdata = X.class)
    expect_identical(out1, out2)

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

test_that("caretList supports combined regression, binary, multiclass", {
  set.seed(42L)

  myControl_reg <- trainControl(
    method = "cv",
    number = 5L,
    savePredictions = "final"
  )

  myControl_bin <- trainControl(
    method = "cv",
    number = 10L,
    savePredictions = "final",
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )

  myControl_multi <- trainControl(
    method = "cv",
    number = 10L,
    savePredictions = "final",
    classProbs = TRUE
  )

  x <- caretList(
    Sepal.Length ~ Sepal.Width,
    iris,
    methodList = c("glm", "lm"),
    trControl = myControl_reg
  )
  expect_is(predict(x), "data.table")

  # Add an rpart model
  x$rpart <- train(Species ~ Sepal.Width, iris, method = "rpart", trControl = myControl_multi)
  expect_is(sapply(x, extractModelType), "character")
  expect_is(predict(x), "data.table")

  set.seed(42L)
  x <- caretList(
    iris[1L:100L, -5L],
    factor(ifelse(iris[1L:100L, "Species"] == "setosa", "Yes", "No")),
    metric = "ROC",
    methodList = c("lda", "rf"),
    trControl = myControl_bin
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, iris, method = "rpart", trControl = myControl_multi)
  expect_is(sapply(x, extractModelType), "character")
})

test_that("extractModelType shouldn't care about predictions", {
  model_list <- models.class
  model_list[[1L]]$pred <- NULL
  expect_is(sapply(model_list, extractModelType), "character")
  expect_equal(unique(sapply(model_list, extractModelType)), "Classification")
})

test_that("extractModelType stops when a classification model can't predict probabilities", {
  model_list <- models.class
  model_list[[1L]]$modelInfo$prob <- FALSE
  err <- "No probability function found. Re-fit with a method that supports prob."
  expect_error(lapply(model_list, extractModelType), err)
})

test_that("extractModelType stops when a classification model did not save probs", {
  model_list <- models.class
  model_list[[1L]]$control$classProbs <- FALSE
  err <- "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl."
  expect_error(lapply(model_list, extractModelType, validate_for_stacking = TRUE), err)
  context("Test helper functions for multiclass classification")

  test_that("Check errors in caretEnsemble for multiclass classification work", {
    data(iris)
    myControl <- trainControl(
      method = "cv",
      number = 5L,
      savePredictions = "final",
      classProbs = TRUE,
      index = createResample(iris[, 5L], 5L)
    )
    model_list <- caretList(
      x = iris[, -5L],
      y = iris[, 5L],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    err <- "caretEnsemble only supports binary classification problems"
    expect_error(check_binary_classification(model_list), err)
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
    wrn <- "classification excluded level is not an integer:"
    expect_warning(expect_equal(validateExcludedClass(0.0), 0L), wrn)
    expect_warning(expect_equal(validateExcludedClass(1.0), 1L), wrn)
    expect_warning(expect_equal(validateExcludedClass(4.0), 4L), wrn)

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

    # Stacking with too great of a level should work. No error or warning.
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
  wrn <- "classification excluded level is not an integer:"
  expect_warning(expect_error(validateExcludedClass(invalid_input), err), wrn)
})

test_that("validateExcludedClass warns for non-integer input", {
  expect_equal(
    expect_warning(
      validated <- validateExcludedClass(1.1),
      "classification excluded level is not an integer: 1.1"
    ), 1L
  )
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
  expect_error(wtd.sd(x, w[-1L]), "'x' and 'w' must have the same length")
})

test_that("extractModelType validates caretList correctly", {
  expect_is(sapply(models.class, extractModelType), "character")
  expect_is(sapply(models.reg, extractModelType), "character")

  # Test error for non-caretList object
  expect_error(
    extractModelType(list(model = lm(Y.reg ~ ., data = as.data.frame(X.reg)))),
    "is(object, \"train\") is not TRUE",
    fixed = TRUE
  )
})

test_that("extractModelType validates model types correctly", {
  expect_is(sapply(models.class, extractModelType), "character")
  expect_is(sapply(models.reg, extractModelType), "character")

  # Test error for mixed model types
  mixed_list <- c(models.class, models.reg)
  expect_is(sapply(mixed_list, extractModelType), "character")
})

test_that("Stacked predictions for caret lists works", {
  best_preds_class <- predict(models.class)
  best_preds_reg <- predict(models.reg)

  expect_is(best_preds_class, "data.table")
  expect_is(best_preds_reg, "data.table")

  expect_named(best_preds_class, names(models.class))
  expect_named(best_preds_reg, names(models.reg))
})

test_that("Stacked predictions works with different resampling strategies", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$Resample <- "WEIRD_SAMPLING"
  expect_is(predict(models.class.inconsistent), "data.table")
})

test_that("Stacked predictions works if the row indexes differ", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$rowIndex <- rev(models.class.inconsistent[[1L]]$pred$rowIndex)
  big_preds <- rbind(models.class.inconsistent[[2L]]$pred, models.class.inconsistent[[2L]]$pred)
  models.class.inconsistent[[2L]]$pred <- big_preds
  expect_is(predict(models.class.inconsistent), "data.table")
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
  expect_identical(unique(sapply(models.class, extractModelType)), "Classification")
  expect_identical(unique(sapply(models.reg, extractModelType)), "Regression")
})

test_that("caretPredict extracts best predictions correctly", {
  stacked_preds_class <- caretPredict(models.class[[1L]], excluded_class_id = 0L)
  stacked_preds_reg <- caretPredict(models.reg[[1L]])

  expect_s3_class(stacked_preds_class, "data.table")
  expect_s3_class(stacked_preds_reg, "data.table")

  expect_named(stacked_preds_class, c("No", "Yes"))
  expect_named(stacked_preds_reg, "pred")
})

test_that("Stacked predictions creates prediction-observation data correctly", {
  stacked_preds_class <- predict(models.class)
  stacked_preds_reg <- predict(models.reg)

  expect_s3_class(stacked_preds_class, "data.table")
  expect_s3_class(stacked_preds_reg, "data.table")

  expect_equal(ncol(stacked_preds_class), length(models.class))
  expect_equal(ncol(stacked_preds_reg), length(models.reg))

  expect_named(stacked_preds_class, names(models.class))
  expect_named(stacked_preds_reg, names(stacked_preds_reg))

  expect_identical(nrow(stacked_preds_class), 150L)
  expect_identical(nrow(stacked_preds_reg), 150L)
})

test_that("Stacked predictions works on new model types, assuming the new type returns a single column called 'pred'", {
  models.class.new <- models.reg
  for (idx in seq_along(models.class.new)) {
    models.class.new[[idx]]$modelType <- "TimeSeries"
  }
  preds <- predict(models.class.new)
  expect_s3_class(preds, "data.table")
})

test_that("validateExcludedClass validates excluded level correctly", {
  expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  expect_error(validateExcludedClass(c(1L, 2L)), "classification excluded level must have a length of 1: length=2")
  expect_error(validateExcludedClass("a"), "classification excluded level must be numeric: a")
  expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
  expect_warning(
    expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"),
    "classification excluded level is not an integer"
  )
  expect_warning(
    expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"),
    "classification excluded level is not an integer"
  )
  expect_warning(validateExcludedClass(1.5), "classification excluded level is not an integer: 1.5")
  txt <- "classification excluded level is not an integer: 2"
  expect_warning(expect_equal(validateExcludedClass(2.0), 2L), txt, "classification excluded level is not an integer")
})

test_that("validateExcludedClass validates excluded level correctly", {
  expect_warning(validateExcludedClass(NULL), "No excluded_class_id set. Setting to 1L.")
  expect_error(validateExcludedClass(c(1L, 2L)), "classification excluded level must have a length of 1: length=2")
  expect_error(validateExcludedClass("a"), "classification excluded level must be numeric: a")
  expect_error(validateExcludedClass(-1L), "classification excluded level must be >= 0: -1")
  expect_warning(
    expect_error(validateExcludedClass(-0.000001), "classification excluded level must be >= 0: -1e-06"),
    "classification excluded level is not an integer"
  )
  expect_warning(
    expect_error(validateExcludedClass(Inf), "classification excluded level must be finite: Inf"),
    "classification excluded level is not an integer"
  )
  expect_warning(validateExcludedClass(1.5), "classification excluded level is not an integer: 1.5")
  txt <- "classification excluded level is not an integer: 2"
  expect_warning(expect_equal(validateExcludedClass(2.0), 2L), txt)
})

test_that("extractModelType fails for models without object$control$savePredictions", {
  model <- models.class[[1L]]
  model$control$savePredictions <- NULL
  err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
  expect_error(extractModelType(model), err)
  model$control$savePredictions <- "BAD_VALUE"
  expect_error(extractModelType(model), err)
})
