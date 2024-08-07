########################################################################
testthat::context("Do the helper functions work for regression objects?")
########################################################################

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

testthat::test_that("Recycling generates a warning", {
  testthat::expect_error(
    caretEnsemble::wtd.sd(matrix(1L:10L, ncol = 2L), w = 1L),
    "'x' and 'w' must have the same length"
  )
})

testthat::test_that("No predictions generates an error", {
  models_multi <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm")
  )
  testthat::expect_is(vapply(models_multi, extractModelType, character(1L)), "character")

  models <- caretList(
    iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
    tuneLength = 1L, verbose = FALSE,
    methodList = c("rf", "gbm")
  )
  new_model <- caret::train(
    iris[, 1L:2L], factor(ifelse(iris[, 5L] == "setosa", "Yes", "No")),
    tuneLength = 1L,
    method = "glmnet",
    metric = "ROC",
    trControl = caret::trainControl(
      method = "cv",
      number = 2L,
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      savePredictions = "final"
    )
  )
  models2 <- c(new_model, models)
  models3 <- c(models, new_model)
  testthat::expect_is(vapply(models, extractModelType, character(1L)), "character")
  testthat::expect_is(vapply(models2, extractModelType, character(1L)), "character")
  testthat::expect_is(vapply(models3, extractModelType, character(1L)), "character")
})

testthat::test_that("We can make the stacked predictions matrix", {
  out <- predict(models.reg)
  testthat::expect_s3_class(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L))
  testthat::expect_named(out, c("rf", "glm", "rpart", "treebag"))
})

testthat::test_that("We can predict", {
  out <- predict(models.reg, newdata = X.reg)
  testthat::expect_is(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L))
  testthat::expect_named(out, c("rf", "glm", "rpart", "treebag"))
})

########################################################################
testthat::context("Do the helper functions work for classification objects?")
########################################################################

testthat::test_that("We can make the stacked predictions matrix", {
  out <- predict(models.class)
  testthat::expect_s3_class(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L * 1L)) # number of models * (number of classes-1)
})

testthat::test_that("We can predict", {
  out <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  testthat::expect_is(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L * 2L))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  testthat::expect_named(out, paste(combinations$Var2, combinations$Var1, sep = "_"))
  out2 <- predict(models.reg, newdata = X.reg)
  testthat::expect_identical(dim(out2), c(150L, 4L))
  testthat::expect_named(out2, c("rf", "glm", "rpart", "treebag"))
})

testthat::test_that("predict results same regardless of verbose option", {
  invisible(capture.output({
    testthat::expect_is(predict(models.class, newdata = X.class), "data.table")
    out1 <- predict(models.class, newdata = X.class)
    out2 <- predict(models.class, verbose = TRUE, newdata = X.class)
    testthat::expect_identical(out1, out2)

    testthat::expect_is(predict(models.reg, newdata = X.reg), "data.table")
    out1 <- predict(models.reg, newdata = X.reg)
    out2 <- predict(models.reg, verbose = TRUE, newdata = X.reg)
    testthat::expect_identical(out1, out2)
  }))
})

testthat::context("Test weighted standard deviations")

testthat::test_that("wtd.sd applies weights correctly", {
  x1 <- c(3L, 5L, 9L, 3L, 4L, 6L, 4L)
  x2 <- c(10L, 10L, 20L, 14L, 2L, 2L, 40L)
  x3 <- c(10L, 10L, 10L, 20L)
  w1 <- c(0.1, 0.1, 0.1, 0.7)
  testthat::expect_error(caretEnsemble::wtd.sd(x1), 'argument "w" is missing, with no default')
  testthat::expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  testthat::expect_false(sd(x1) == caretEnsemble::wtd.sd(x1, w = x2))
  testthat::expect_equal(caretEnsemble::wtd.sd(x3, w = w1), 5.291503, tolerance = 0.001)
  testthat::expect_equal(caretEnsemble::wtd.sd(x3, w = w1 * 100L), caretEnsemble::wtd.sd(x3, w = w1), tolerance = 0.001)
})

testthat::test_that("wtd.sd handles NA values correctly", {
  x1 <- c(10L, 10L, 10L, 20L, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  testthat::expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  testthat::expect_true(is.na(sd(x1)))
  testthat::expect_false(is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
  testthat::expect_false(is.na(sd(x1, na.rm = TRUE)))
  testthat::expect_true(is.na(caretEnsemble::wtd.sd(x1, w = w1)))
  testthat::expect_false(is.na(caretEnsemble::wtd.sd(x1, w = w1, na.rm = TRUE)))
})

testthat::test_that("caretList supports combined regression, binary, multiclass", {
  set.seed(42L)

  # Regression models
  reg_models <- caretList(
    Sepal.Length ~ Sepal.Width,
    iris,
    methodList = c("glm", "lm")
  )
  testthat::expect_is(predict(reg_models), "data.table")

  # Binary model
  bin_models <- caretList(
    factor(ifelse(Species == "setosa", "Yes", "No")) ~ Sepal.Width,
    iris,
    methodList = c("lda", "rpart")
  )
  testthat::expect_is(predict(bin_models), "data.table")

  # Multiclass model
  multi_models <- caretList(
    Species ~ Sepal.Width,
    iris,
    methodList = "rpart"
  )
  testthat::expect_is(predict(multi_models), "data.table")

  # Combine them!
  all_models <- c(reg_models, bin_models, multi_models)
  testthat::expect_s3_class(all_models, "caretList")
  testthat::expect_is(vapply(all_models, extractModelType, character(1L)), "character")

  # Test preds
  stacked_p <- predict(all_models)
  new_p <- predict(all_models, newdata = iris[seq_len(10L), ])
  testthat::expect_is(stacked_p, "data.table")
  testthat::expect_is(new_p, "data.table")
  testthat::expect_identical(nrow(stacked_p), nrow(iris))
  testthat::expect_identical(nrow(new_p), 10L)
})

testthat::test_that("extractModelType shouldn't care about predictions", {
  model_list <- models.class
  model_list[[1L]]$pred <- NULL
  testthat::expect_is(vapply(model_list, extractModelType, character(1L)), "character")
  testthat::expect_equivalent(unique(vapply(model_list, extractModelType, character(1L))), "Classification")
})

testthat::test_that("extractModelType stops when a classification model can't predict probabilities", {
  model_list <- models.class
  model_list[[1L]]$modelInfo$prob <- FALSE
  err <- "No probability function found. Re-fit with a method that supports prob."
  testthat::expect_error(lapply(model_list, extractModelType), err)
})

testthat::test_that("extractModelType stops when a classification model did not save probs", {
  model_list <- models.class
  model_list[[1L]]$control$classProbs <- FALSE
  err <- "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl."
  testthat::expect_error(lapply(model_list, extractModelType, validate_for_stacking = TRUE), err)
  testthat::context("Test helper functions for multiclass classification")

  testthat::test_that("Check errors in caretEnsemble for multiclass classification work", {
    data(iris)
    model_list <- caretList(
      x = iris[, -5L],
      y = iris[, 5L],
      methodList = c("rpart", "glmnet")
    )

    err <- "caretEnsemble only supports binary classification problems"
    testthat::expect_error(check_binary_classification(model_list), err)
    testthat::expect_null(check_binary_classification(models.class))
    testthat::expect_null(check_binary_classification(models.reg))

    # Do not produce errors when another object is passed
    testthat::expect_null(check_binary_classification(NULL))
    testthat::expect_null(check_binary_classification(2L))
    testthat::expect_null(check_binary_classification(list("string")))
    testthat::expect_null(check_binary_classification(iris))
  })

  testthat::test_that("Configuration function for excluded level work", {
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
    # Should also validate it?
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
})

# Tests for validateExcludedClass function
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

########################################################################
testthat::context("Helper function edge cases")
########################################################################

testthat::test_that("wtd.sd calculates weighted standard deviation correctly", {
  x <- c(1L, 2L, 3L, 4L, 5L)
  w <- c(1L, 1L, 1L, 1L, 1L)
  testthat::expect_equal(wtd.sd(x, w), sd(x), tol = 0.001)

  w <- c(2L, 1L, 1L, 1L, 1L)
  testthat::expect_true(wtd.sd(x, w) != sd(x))

  # Test with NA values
  x_na <- c(1L, 2L, NA, 4L, 5L)
  testthat::expect_true(is.na(wtd.sd(x_na, w)))
  testthat::expect_false(is.na(wtd.sd(x_na, w, na.rm = TRUE)))

  # Test error for mismatched lengths
  testthat::expect_error(wtd.sd(x, w[-1L]), "'x' and 'w' must have the same length")
})

testthat::test_that("extractModelType validates caretList correctly", {
  testthat::expect_is(vapply(models.class, extractModelType, character(1L)), "character")
  testthat::expect_is(vapply(models.reg, extractModelType, character(1L)), "character")

  # Test error for non-caretList object
  testthat::expect_error(
    extractModelType(list(model = lm(Y.reg ~ ., data = as.data.frame(X.reg)))),
    "is(object, \"train\") is not TRUE",
    fixed = TRUE
  )
})

testthat::test_that("extractModelType validates model types correctly", {
  testthat::expect_is(vapply(models.class, extractModelType, character(1L)), "character")
  testthat::expect_is(vapply(models.reg, extractModelType, character(1L)), "character")

  # Test error for mixed model types
  mixed_list <- c(models.class, models.reg)
  testthat::expect_is(vapply(mixed_list, extractModelType, character(1L)), "character")
})

testthat::test_that("Stacked predictions for caret lists works", {
  best_preds_class <- predict(models.class)
  best_preds_reg <- predict(models.reg)

  testthat::expect_is(best_preds_class, "data.table")
  testthat::expect_is(best_preds_reg, "data.table")

  testthat::expect_named(best_preds_class, names(models.class))
  testthat::expect_named(best_preds_reg, names(models.reg))
})

testthat::test_that("Stacked predictions works with different resampling strategies", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$Resample <- "WEIRD_SAMPLING"
  testthat::expect_is(predict(models.class.inconsistent), "data.table")
})

testthat::test_that("Stacked predictions works if the row indexes differ", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$rowIndex <- rev(models.class.inconsistent[[1L]]$pred$rowIndex)
  big_preds <- rbind(models.class.inconsistent[[2L]]$pred, models.class.inconsistent[[2L]]$pred)
  models.class.inconsistent[[2L]]$pred <- big_preds
  testthat::expect_is(predict(models.class.inconsistent), "data.table")
})

testthat::test_that("extractModelName extracts model names correctly", {
  testthat::expect_identical(extractModelName(models.class[[1L]]), "rf")
  testthat::expect_identical(extractModelName(models.reg[[1L]]), "rf")

  # Test custom model
  custom_model <- models.class[[1L]]
  custom_model$method <- list(method = "custom_rf")
  testthat::expect_identical(extractModelName(custom_model), "custom_rf")
})

testthat::test_that("extractModelType extracts model types correctly", {
  testthat::expect_identical(unique(vapply(models.class, extractModelType, character(1L))), "Classification")
  testthat::expect_identical(unique(vapply(models.reg, extractModelType, character(1L))), "Regression")
})

testthat::test_that("caretPredict extracts best predictions correctly", {
  stacked_preds_class <- caretPredict(models.class[[1L]], excluded_class_id = 0L)
  stacked_preds_reg <- caretPredict(models.reg[[1L]])

  testthat::expect_s3_class(stacked_preds_class, "data.table")
  testthat::expect_s3_class(stacked_preds_reg, "data.table")

  testthat::expect_named(stacked_preds_class, c("No", "Yes"))
  testthat::expect_named(stacked_preds_reg, "pred")
})

testthat::test_that("Stacked predictions creates prediction-observation data correctly", {
  stacked_preds_class <- predict(models.class)
  stacked_preds_reg <- predict(models.reg)

  testthat::expect_s3_class(stacked_preds_class, "data.table")
  testthat::expect_s3_class(stacked_preds_reg, "data.table")

  testthat::expect_identical(ncol(stacked_preds_class), length(models.class))
  testthat::expect_identical(ncol(stacked_preds_reg), length(models.reg))

  testthat::expect_named(stacked_preds_class, names(models.class))
  testthat::expect_named(stacked_preds_reg, names(stacked_preds_reg))

  testthat::expect_identical(nrow(stacked_preds_class), 150L)
  testthat::expect_identical(nrow(stacked_preds_reg), 150L)
})

testthat::test_that("Stacked predictions works on new model types", {
  # Note that new model types would have to return a single column called 'pred'
  models.class.new <- models.reg
  for (idx in seq_along(models.class.new)) {
    models.class.new[[idx]]$modelType <- "TimeSeries"
  }
  preds <- predict(models.class.new)
  testthat::expect_s3_class(preds, "data.table")
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

testthat::test_that("extractModelType fails for models without object$control$savePredictions", {
  model <- models.class[[1L]]
  model$control$savePredictions <- NULL
  err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
  testthat::expect_error(extractModelType(model), err)
  model$control$savePredictions <- "BAD_VALUE"
  testthat::expect_error(extractModelType(model), err)
})
