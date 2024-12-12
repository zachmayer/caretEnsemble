# Load data and create models
utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)
utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)
utils::data(iris)

models_multiclass <- caretList(
  x = iris[, -5L],
  y = iris[, 5L],
  methodList = c("rpart", "glmnet")
)

ens.class <- caretStack(models.class, preProcess = "pca", method = "glm", metric = "ROC")

ens.reg <- caretStack(models.reg, preProcess = "pca", method = "lm")

# Helper functions
expect_all_finite <- function(x) {
  testthat::expect_true(all(vapply(x, function(col) all(is.finite(col)), logical(1L))))
}

######################################################################
testthat::context("caretStack")
######################################################################

testthat::test_that("caretStack creates valid ensemble models", {
  testthat::expect_s3_class(ens.class, "caretStack")
  testthat::expect_s3_class(ens.reg, "caretStack")

  testthat::expect_s3_class(summary(ens.class), "summary.caretStack")
  testthat::expect_s3_class(plot(ens.class), "ggplot")
  testthat::expect_output(print(ens.class), "The following models were ensembled: rf, glm, rpart, treebag")
})

testthat::test_that("predict works for classification and regression ensembles", {
  ens_list <- list(class = ens.class, reg = ens.reg)
  X_list <- list(class = X.class, reg = X.reg)

  for (model_name in names(ens_list)) {
    ens <- ens_list[[model_name]]
    X <- X_list[[model_name]]

    param_grid <- expand.grid(
      se = c(TRUE, FALSE),
      newdata = c(TRUE, FALSE)
    )

    for (i in seq_len(nrow(param_grid))) {
      params <- param_grid[i, ]
      newdata <- if (params$newdata) X[1L:50L, ] else NULL
      expected_rows <- if (params$newdata) 50L else nrow(X)

      pred <- predict(ens, newdata = newdata, se = params$se)
      testthat::expect_s3_class(pred, "data.table")
      testthat::expect_identical(nrow(pred), expected_rows)
      expect_all_finite(pred)
    }
  }
})

testthat::test_that("Classification predictions return correct output", {
  pred.class <- predict(ens.class, X.class, return_class_only = TRUE)
  testthat::expect_s3_class(pred.class, "factor")
  testthat::expect_length(pred.class, nrow(X.class))

  testthat::expect_named(
    predict(ens.class, X.class, se = TRUE, excluded_class_id = 1L),
    c("pred", "lwr", "upr")
  )

  testthat::expect_named(
    predict(ens.class, X.class, se = TRUE, excluded_class_id = 0L),
    c("pred.No", "pred.Yes", "lwr.No", "lwr.Yes", "upr.No", "upr.Yes")
  )
})

testthat::test_that("Regression predictions return correct output", {
  testthat::expect_named(
    predict(ens.reg, X.reg, se = TRUE, excluded_class_id = 0L),
    c("pred", "lwr", "upr")
  )
})

testthat::test_that("Predictions are reproducible", {
  set.seed(42L)
  p1 <- predict(ens.class, X.class, se = TRUE, level = 0.8)
  set.seed(42L)
  p2 <- predict(ens.class, X.class, se = TRUE, level = 0.8)

  testthat::expect_equivalent(p1, p2)
})

testthat::test_that("caretStack handles missing data correctly", {
  ens.reg.subset <- caretEnsemble::caretStack(models.reg[2L:3L], method = "lm")

  X_reg_na <- X.reg
  X_reg_na[sample.int(nrow(X_reg_na), 20L), sample.int(ncol(X_reg_na) - 1L, 1L)] <- NA

  pred.reg <- predict(ens.reg.subset, newdata = X_reg_na)
  testthat::expect_identical(nrow(pred.reg), nrow(X_reg_na))

  pred.reg <- predict(ens.reg.subset, newdata = X_reg_na)
  testthat::expect_identical(nrow(pred.reg), nrow(X_reg_na))
})

testthat::test_that("caretStack handles multiclass problems", {
  meta_model <- caretEnsemble::caretStack(models_multiclass, method = "rpart", excluded_class_id = 4L)
  pred <- predict(meta_model, newdata = iris)
  testthat::expect_identical(nrow(pred), 150L)
  testthat::expect_identical(ncol(pred), 3L)
  expect_all_finite(pred)
})

testthat::test_that("caretStack works with different stacking algorithms", {
  stack_methods <- c("glm", "rf", "gbm", "glmnet")

  for (method in stack_methods) {
    for (model_list in list(models.reg, models.class)) {
      stack <- if (method == "gbm") {
        caretEnsemble::caretStack(model_list, method = method, verbose = FALSE)
      } else {
        caretEnsemble::caretStack(model_list, method = method)
      }

      testthat::expect_s3_class(stack, "caretStack")
      testthat::expect_identical(stack$ens_model$method, method)

      predictions <- predict(stack, newdata = if (identical(model_list, models.reg)) X.reg else X.class)
      testthat::expect_identical(nrow(predictions), nrow(if (identical(model_list, models.reg)) X.reg else X.class))
    }
  }
})

testthat::test_that("caretStack handles custom preprocessing", {
  preprocess <- c("center", "scale", "pca")
  for (model_list in list(models.class, models.reg)) {
    stack <- caretEnsemble::caretStack(model_list, method = "glm", preProcess = preprocess)
    testthat::expect_s3_class(stack, "caretStack")
    testthat::expect_named(stack$ens_model$preProcess$method, c(preprocess, "ignore"))
  }
})

testthat::test_that("caretStack handles custom performance function", {
  custom_summary <- function(data, lev = NULL, model = NULL) {
    c(default = mean(data$obs == data$pred))
  }

  for (model_list in list(models.class, models.reg)) {
    stack <- caretEnsemble::caretStack(
      model_list,
      method = "glm",
      metric = "default",
      trControl = caret::trainControl(method = "cv", number = 3L, summaryFunction = custom_summary)
    )

    testthat::expect_s3_class(stack, "caretStack")
    testthat::expect_true("default" %in% names(stack$ens_model$results))
  }
})

testthat::test_that("caretStack handles new data correctly", {
  set.seed(42L)
  N <- 50L
  idx <- sample.int(nrow(X.reg), N)

  stack_class <- caretStack(
    models.class,
    metric = "ROC",
    method = "rpart",
    new_X = X.class[idx, ],
    new_y = Y.class[idx]
  )

  stack_reg <- caretStack(
    models.reg,
    method = "glm",
    new_X = X.reg[idx, ],
    new_y = Y.reg[idx]
  )

  testthat::expect_s3_class(stack_class, "caretStack")
  testthat::expect_s3_class(stack_reg, "caretStack")

  pred_class <- predict(stack_class, X.class)
  pred_reg <- predict(stack_reg, X.reg)

  testthat::expect_s3_class(pred_class, "data.table")
  testthat::expect_s3_class(pred_reg, "data.table")

  testthat::expect_identical(nrow(pred_class), nrow(X.class))
  testthat::expect_identical(nrow(pred_reg), nrow(X.reg))

  testthat::expect_identical(ncol(pred_class), 2L)
  testthat::expect_identical(ncol(pred_reg), 1L)
})

testthat::test_that("caretStack handles original all original features correctly", {
  model_stack_1 <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    use_original_features = TRUE,
    which.var = colnames(X.class)
  )

  # Check that the internal variable is correctly set
  testthat::expect_identical(model_stack_1$original_features_included, colnames(X.class))

  # Check we can predict
  pred_1 <- predict(model_stack_1, newdata = X.class)
  testthat::expect_s3_class(pred_1, "data.table")
  testthat::expect_identical(nrow(pred_1), nrow(X.class))
  expect_all_finite(pred_1)

  # Do not specify which.var (by default, all columns are used)
  model_stack_2 <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    use_original_features = TRUE
  )

  # Check that all columns are used
  testthat::expect_identical(model_stack_2$original_features_included, colnames(X.class))

  # Check we can predict
  pred_2 <- predict(model_stack_2, newdata = X.class)
  testthat::expect_s3_class(pred_2, "data.table")
  testthat::expect_identical(nrow(pred_2), nrow(X.class))
  expect_all_finite(pred_2)
})

testthat::test_that("caretStack handles a subset of original features given its names correctly", {
  model_stack <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    use_original_features = TRUE,
    which.var = c("Sepal.Width", "Petal.Length")
  )

  # Check that the internal variable is correctly set
  testthat::expect_identical(model_stack$original_features_included, c("Sepal.Width", "Petal.Length"))

  # Check we can predict
  pred <- predict(model_stack, newdata = X.class)
  testthat::expect_s3_class(pred, "data.table")
  testthat::expect_identical(nrow(pred), nrow(X.class))
  expect_all_finite(pred)
})

testthat::test_that("caretStack handles a subset of original features given its indexes", {
  model_stack <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    use_original_features = TRUE,
    which.var = c(2L, 3L)
  )

  # Check that the extracted columns are the correct ones
  testthat::expect_identical(model_stack$original_features_included, colnames(X.class)[c(2L, 3L)])

  # Check we can predict
  pred <- predict(model_stack, newdata = X.class)
  testthat::expect_s3_class(pred, "data.table")
  testthat::expect_identical(nrow(pred), nrow(X.class))
})

testthat::test_that("We can get variable importance when use_original_features is true", {
  model_stack <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    use_original_features = TRUE,
    which.var = c("Sepal.Width", "Petal.Length")
  )
  imp <- caret::varImp(model_stack, normalize = TRUE, newdata = X.class)

  testthat::expect_named(imp, c("rf", "glm", "rpart", "treebag", "Sepal.Width", "Petal.Length"))
})

testthat::test_that("caretStack coerces lists to caretLists", {
  models <- list(
    models.class[[1L]],
    models.class[[2L]]
  )
  testthat::expect_warning(
    caretStack(models, method = "glm", tuneLength = 1L),
    "Attempting to coerce all.models to a caretList."
  )
})

testthat::test_that("caretStack errors if new_X is provided but not new_y", {
  testthat::expect_error(
    caretStack(models.class, new_X = X.class),
    "Both new_X and new_y must be NULL, or neither."
  )
})

testthat::test_that("caretStack errors if new_y is provided but not new_X", {
  testthat::expect_error(
    caretStack(models.class, new_y = Y.class),
    "Both new_X and new_y must be NULL, or neither."
  )
})

testthat::test_that("caretStack warns if use_original_features is provided, but new_X is NULL", {
  testthat::expect_warning(
    caretStack(models.class, use_original_features = TRUE, which.var = c("Sepal.Width", "Petal.Length")),
    "use_original_features is ignored when new_X is NULL."
  )
})

testthat::test_that("caretStack warns if which.var is provided, but use_original_features is FALSE", {
  testthat::expect_warning(
    caretStack(
      models.class,
      use_original_features = FALSE, which.var = c("Sepal.Width", "Petal.Length")
    ),
    "which.var is ignored when use_original_features is FALSE."
  )
})

testthat::test_that("caretStack warns if which.var contains invalid columns", {
  # Some other warning messages can be printed due to colinear columns when new_X and new_y are provided
  warning_messages <- capture_warnings({
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      use_original_features = TRUE, which.var = c("Sepal.Width", "Wrong column1", "Wrong column2")
    )
  })

  expected_warning_message <- paste0(
    "The following variables are not in new_X and will be ignored: ",
    "Wrong column1, Wrong column2. Ignoring these variables."
  )
  testthat::expect_true(expected_warning_message %in% warning_messages)
})

testthat::test_that("caretStack warns if which.var contains invalid indexes", {
  # Some other warning messages can be printed due to colinear columns when new_X and new_y are provided
  warning_messages <- capture_warnings({
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      use_original_features = TRUE, which.var = c(0L, 1L, 2L, 3L)
    )
  })

  expected_warning_message <- "which.var contains indexes that are less or equal than 0. Ignoring these indexes."
  testthat::expect_true(expected_warning_message %in% warning_messages)

  warning_messages <- capture_warnings({
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      use_original_features = TRUE, which.var = c(1L, 2L, 3L, 9L)
    )
  })

  max_index <- ncol(X.class)
  expected_warning_message <- paste0(
    "which.var contains indexes that exceed the number of columns in new_X. Maximum allowed index is ",
    max_index, ". Indexes will be truncated."
  )
  testthat::expect_true(expected_warning_message %in% warning_messages)
})

testthat::test_that("caretStack errors when which.var is not a character, integer vector or NULL", {
  testthat::expect_error(
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      use_original_features = TRUE,
      which.var = list("Sepal.Width", "Petal.Length")
    ),
    "which.var must be a character vector, a numeric vector or NULL when use_original_features is TRUE."
  )
})

testthat::test_that("caretStack errors when call predict and newdata colnames are not the same used in training", {
  model_stack <- caretStack(
    models.class,
    new_X = X.class, new_y = Y.class,
    use_original_features = TRUE,
    which.var = colnames(X.class)
  )
  newdata <- X.class
  colnames(newdata) <- c(
    "Wrong column1", "Wrong column2", "Wrong column3",
    "Petal.Width", "Speciesversicolor", "Speciesvirginica"
  )
  error_message <- paste0(
    "newdata does not contain all the original features used in training. ",
    "Missing columns: Wrong column1, Wrong column2, Wrong column3."
  )

  testthat::expect_error(predict(model_stack, newdata = newdata), error_message)
})

testthat::test_that("caretStack errors when call varImp and newdata is null", {
  model_stack <- caretStack(
    models.class,
    new_X = X.class, new_y = Y.class,
    use_original_features = TRUE,
    which.var = colnames(X.class)
  )

  error_message <- paste0(
    "Original features were used in training, but they cannot be used if newdata is NULL. ",
    "Please provide newdata to compute variable importance."
  )
  testthat::expect_error(caret::varImp(model_stack), error_message)
})

######################################################################
testthat::context("S3 methods for caretStack")
######################################################################

testthat::test_that("print", {
  for (ens in list(ens.class, ens.reg)) {
    testthat::expect_output(print(ens), "The following models were ensembled: rf, glm, rpart, treebag")
    testthat::expect_output(print(ens), "Linear")
    testthat::expect_output(print(ens), "Resampling: Cross-Validated (5 fold)", fixed = TRUE)
  }
})

testthat::test_that("summary", {
  for (ens in list(ens.class, ens.reg)) {
    s <- summary(ens)
    testthat::expect_s3_class(s, "summary.caretStack")
    testthat::expect_output(print(s), "The following models were ensembled: rf, glm, rpart, treebag")
    testthat::expect_output(print(s), "Model Importance:")
    testthat::expect_output(print(s), "Model accuracy:")
  }
})

testthat::test_that("plot", {
  for (ens in list(ens.class, ens.reg)) {
    p <- plot(ens)
    testthat::expect_s3_class(p, "ggplot")
  }
})

testthat::test_that("dotplot", {
  for (ens in list(ens.class, ens.reg)) {
    p <- lattice::dotplot(ens)
    testthat::expect_s3_class(p, "trellis")
  }
})

testthat::test_that("autoplot", {
  for (ens in list(ens.class, ens.reg)) {
    p <- ggplot2::autoplot(ens, X.reg)
    testthat::expect_s3_class(p, "ggplot")
  }
})

######################################################################
testthat::context("varImp")
######################################################################

testthat::test_that("varImp works for classification and regression", {
  for (ens in list(ens.class, ens.reg)) {
    imp <- caret::varImp(ens)
    testthat::expect_type(imp, "double")
    testthat::expect_named(imp, names(ens$models))
    testthat::expect_equal(sum(imp), 1.0, tolerance = 1e-6)

    imp_new_data <- caret::varImp(ens, if (identical(ens, ens.class)) X.class else X.reg)
    testthat::expect_type(imp_new_data, "double")
    testthat::expect_named(imp_new_data, names(ens$models))
    testthat::expect_equal(sum(imp_new_data), 1.0, tolerance = 1e-6)
  }
})

######################################################################
testthat::context("wtd.sd")
######################################################################

testthat::test_that("wtd.sd calculates weighted standard deviation correctly", {
  x <- c(1L, 2L, 3L, 4L, 5L)
  w <- c(1L, 1L, 1L, 1L, 1L)
  testthat::expect_equal(caretEnsemble::wtd.sd(x, w), stats::sd(x), tolerance = 0.001)

  w_uneven <- c(2L, 1L, 1L, 1L, 1L)
  testthat::expect_false(isTRUE(all.equal(caretEnsemble::wtd.sd(x, w_uneven), stats::sd(x))))

  x_na <- c(1L, 2L, NA, 4L, 5L)
  testthat::expect_true(is.na(caretEnsemble::wtd.sd(x_na, w)))
  testthat::expect_false(is.na(caretEnsemble::wtd.sd(x_na, w, na.rm = TRUE)))

  testthat::expect_error(caretEnsemble::wtd.sd(x, w[-1L]), "'x' and 'w' must have the same length")

  x3 <- c(10L, 10L, 10L, 20L)
  w1 <- c(0.1, 0.1, 0.1, 0.7)
  testthat::expect_equal(caretEnsemble::wtd.sd(x3, w = w1), 5.291503, tolerance = 0.001)
  testthat::expect_equal(caretEnsemble::wtd.sd(x3, w = w1 * 100L), caretEnsemble::wtd.sd(x3, w = w1), tolerance = 0.001)
})

######################################################################
testthat::context("set_excluded_class_id")
######################################################################

testthat::test_that("set_excluded_class_id warning if unset", {
  old_ensemble <- ens.class
  old_ensemble$excluded_class_id <- NULL

  is_class <- isClassifier(old_ensemble)
  new_ensemble <- expect_warning(
    set_excluded_class_id(old_ensemble, is_class),
    "No excluded_class_id set. Setting to 1L."
  )

  testthat::expect_identical(new_ensemble$excluded_class_id, 1L)
})
