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

testthat::test_that("caretStack handles original features correctly", {
  model_stack_1 <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  # Check that the internal variable is correctly set
  testthat::expect_identical(model_stack_1$original_features, colnames(X.class))

  # Check we can predict
  pred_1 <- predict(model_stack_1, newdata = X.class)
  testthat::expect_s3_class(pred_1, "data.table")
  testthat::expect_identical(nrow(pred_1), nrow(X.class))
  expect_all_finite(pred_1)

  # Do not specify original features
  model_stack_2 <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = NULL
  )

  # Check that all columns are used
  testthat::expect_null(model_stack_2$original_features, NULL)
})

testthat::test_that("caretStack handles a subset of original features given its names", {
  model_stack <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = c("Sepal.Width", "Petal.Length")
  )

  # Check that the internal variable is correctly set
  testthat::expect_identical(model_stack$original_features, c("Sepal.Width", "Petal.Length"))

  # Check we can predict
  pred <- predict(model_stack, newdata = X.class)
  testthat::expect_s3_class(pred, "data.table")
  testthat::expect_identical(nrow(pred), nrow(X.class))
  expect_all_finite(pred)
})

testthat::test_that("We can get variable importance when we use original features", {
  model_stack <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = c("Sepal.Width", "Petal.Length")
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

testthat::test_that("caretStack errors if original features is provided, but new_X or newdata is NULL", {
  testthat::expect_error(
    caretStack(models.class, original_features = colnames(X.class)),
    "original_features was specified, but new_X is not provided. Please provide new_X to use original features."
  )

  # Train a model with all original_features
  stack_model <- caretStack(
    models.class,
    new_X = X.class, new_y = Y.class,
    original_features = colnames(X.class)
  )
  # Predict without newdata
  testthat::expect_error(
    predict(stack_model),
    "original_features was specified, but newdata is not provided. Please provide newdata to use original features."
  )
  # Calculate variable importance without newdata
  testthat::expect_error(
    caret::varImp(stack_model),
    "original_features was specified, but newdata is not provided. Please provide newdata to use original features."
  )
})

testthat::test_that("caretStack errors when original_features contains non-existent columns", {
  # Train a model with a bad argument value for original_features
  testthat::expect_error(
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      original_features = c("Sepal.Width", "Petal.Length", "NonExistentColumn1", "NonExistentColumn2")
    ),
    paste0(
      "Not all variables in original_features are present in new_X. Missing columns: ",
      "NonExistentColumn1, NonExistentColumn2."
    )
  )

  # Train a model with all original_features
  stack_model <- caretStack(
    models.class,
    new_X = X.class, new_y = Y.class,
    original_features = colnames(X.class)
  )
  newdata <- X.class
  colnames(newdata) <- c(
    "(Intercept)", "NonExistentColumn1", "NonExistentColumn2", "Petal.Width", "Speciesversicolor", "Speciesvirginica"
  )
  # Predict with bad colnames for newdata
  testthat::expect_error(
    predict(stack_model, newdata = newdata),
    paste0(
      "Not all variables in original_features are present in newdata. Missing columns: ",
      "Sepal.Width, Petal.Length."
    )
  )
  # Calculate variable importance with bad colnames for newdata
  testthat::expect_error(
    caret::varImp(stack_model, newdata = newdata),
    paste0(
      "Not all variables in original_features are present in newdata. Missing columns: ",
      "Sepal.Width, Petal.Length."
    )
  )
})

testthat::test_that("caretStack errors when original features is not a character vector or NULL", {
  testthat::expect_error(
    caretStack(
      models.class,
      new_X = X.class, new_y = Y.class,
      original_features = list("Sepal.Width", "Petal.Length")
    ),
    paste0(
      "original_features must be a character vector with the names of the features to include,",
      " or NULL to not include any features."
    )
  )
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

######################################################################
testthat::context("Aggregate resamples")
######################################################################

testthat::test_that("caretList handles aggregate_resamples correctly", {
  set.seed(42L)

  xvars <- colnames(iris)[2L:5L]
  yvar <- colnames(iris)[1L]

  repeats <- 5L

  models_agg <- caretList(
    x = iris[, xvars],
    y = iris[, yvar],
    methodList = c("glm", "lm"),
    trControl = caret::trainControl(method = "repeatedcv", number = 5L, repeats = repeats),
    aggregate_resamples = TRUE
  )

  models_no_agg <- caretList(
    x = iris[, xvars],
    y = iris[, yvar],
    methodList = c("glm", "lm"),
    trControl = caret::trainControl(method = "repeatedcv", number = 5L, repeats = repeats),
    aggregate_resamples = FALSE
  )

  # Test stacked predictionswith aggregate_resamples = TRUE
  pred_agg <- predict(models_agg, aggregate_resamples = TRUE)
  pred_no_agg <- predict(models_no_agg, aggregate_resamples = FALSE)

  # Aggregated stacked predictions should have the same number of rows as the input data:
  testthat::expect_identical(nrow(pred_agg), nrow(iris))

  # Non-aggregated stacked predictions should input rows X repeats rows
  testthat::expect_identical(nrow(pred_no_agg), nrow(iris) * repeats)

  # Both stacks should have one column per model
  testthat::expect_identical(ncol(pred_agg), 2L)
  testthat::expect_identical(ncol(pred_no_agg), 2L)
})


######################################################################
testthat::context("plot_variable_importance function")
######################################################################

testthat::test_that("plot_variable_importance returns ggplot or patchwork objects", {
  # Train model with original_features (we have two plots)
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  # Case without stat_type
  p <- plot_variable_importance(stack_model, newdata = X.class)
  testthat::expect_s3_class(p, "patchwork")

  # Case with valid stat_type
  p_mean <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "mean")
  testthat::expect_s3_class(p_mean, "patchwork")

  # Case with invalid stat_type: should trigger a warning and be ignored
  testthat::expect_warning(
    {
      p_warn <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "invalid")
    },
    "stat_type must be 'mean', 'sum', or 'max'"
  )
  testthat::expect_s3_class(p_warn, "patchwork")

  # Case without original_features
  stack_model_no_orig <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = NULL
  )
  p_new <- plot_variable_importance(stack_model_no_orig, newdata = X.class)
  testthat::expect_s3_class(p_new, "ggplot")
})


testthat::test_that("plot_variable_importance plots correct titles and colors", {
  stack_model <- caretStack(
    models.class,
    method = "glmnet",
    new_X = X.class,
    new_y = Y.class,
    original_features = colnames(X.class)
  )

  p <- plot_variable_importance(stack_model, newdata = X.class, stat_type = "max")

  # Extract individual plots if it is a patchwork object
  if (inherits(p, "patchwork")) {
    plots <- list(p[[1L]], p[[2L]])
  } else {
    plots <- list(p)
  }

  # Check plot titles
  titles <- vapply(plots, function(g) g$labels$title, FUN.VALUE = character(1L))
  testthat::expect_true(any(grepl("Original Features", titles, fixed = TRUE)))
  testthat::expect_true(any(grepl("New Features", titles, fixed = TRUE)))

  # Check fill colors
  fills <- unique(unlist(lapply(plots, function(g) {
    ggplot2::ggplot_build(g)$data[[1L]]$fill
  })))
  testthat::expect_true(any(grepl("blue", fills, fixed = TRUE)))
  testthat::expect_true(any(grepl("red", fills, fixed = TRUE)))
})
