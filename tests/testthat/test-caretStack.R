data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

data(iris)

models_multiclass <- caretList(
  x = iris[, -5L],
  y = iris[, 5L],
  methodList = c("rpart", "glmnet")
)

ens.class <- caretStack(
  models.class,
  method = "glm",
  preProcess = "pca",
  trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
)

ens.reg <- caretStack(
  models.reg,
  method = "lm",
  preProcess = "pca",
  trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = FALSE)
)

#############################################################################
testthat::context("caretStack")
#############################################################################

testthat::test_that("We can make predictions from stacks, including cases where the stacked model has preprocessing", {
  ens_list <- list(class = ens.class, reg = ens.reg)
  X_list <- list(class = X.class, reg = X.reg)

  for (model_name in names(ens_list)) {
    # S3 methods
    ens <- ens_list[[model_name]]
    testthat::expect_s3_class(ens, "caretStack")
    testthat::expect_s3_class(summary(ens), "summary.caretStack")
    testthat::expect_s3_class(plot(ens), "ggplot")
    invisible(capture.output(print(ens)))

    # Predictions
    param_grid <- expand.grid(
      se = c(TRUE, FALSE),
      newdata = c(TRUE, FALSE)
    )
    for (i in seq_len(nrow(param_grid))) {
      params <- param_grid[i, ]
      X <- X_list[[model_name]]
      expected_rows <- nrow(X)
      newdata <- NULL
      if (params$newdata) {
        newdata <- X[1L:50L, ]
        expected_rows <- 50L
      }
      pred <- predict(ens, newdata = newdata, se = params$se)
      testthat::expect_s3_class(pred, "data.table")
      testthat::expect_identical(nrow(pred), expected_rows)
      testthat::expect_true(all(vapply(pred, is.numeric, logical(1L))))
    }
  }
})

testthat::test_that("For classificaiton, we can predict the class labels", {
  pred.class <- predict(ens.class, X.class, return_class_only = TRUE)
  expect_is(pred.class, "factor")
  expect_length(pred.class, nrow(X.class))
})

testthat::test_that("caretStack plots", {
  ens.gbm <- caretStack(
    models.reg,
    method = "gbm", tuneLength = 2L, verbose = FALSE
  )
  testthat::expect_s3_class(ens.gbm, "caretStack")

  plt <- plot(ens.gbm)
  expect_s3_class(plt, "ggplot")

  dotplot <- lattice::dotplot(ens.gbm, metric = "RMSE")
  expect_s3_class(dotplot, "trellis")
})

testthat::test_that("Prediction names are correct with SE", {
  testthat::expect_named(
    predict(ens.reg, X.reg, se = TRUE, excluded_class_id = 0L),
    c("pred", "lwr", "upr")
  )

  testthat::expect_named(
    predict(ens.class, X.class, se = TRUE, excluded_class_id = 1L),
    c("pred", "lwr", "upr")
  )

  testthat::expect_named(
    predict(ens.class, X.class, se = TRUE, excluded_class_id = 0L),
    c("pred.No", "pred.Yes", "lwr.No", "lwr.Yes", "upr.No", "upr.Yes")
  )
})

testthat::test_that("Prediction equivalence", {
  # Note that SE is stochastic, since it uses permutation importance

  set.seed(42L)
  p1 <- predict(ens.class, X.class, se = TRUE, level = 0.8)
  set.seed(42L)
  p2 <- predict(ens.class, X.class, se = TRUE, level = 0.8)

  testthat::expect_equivalent(p1, p2)
})

testthat::test_that("Test na.action pass through", {
  set.seed(1337L)

  # drop the first model because it does not support na.pass
  ens.reg <- caretStack(models.reg[2L:3L], method = "lm")

  X_reg_na <- X.reg
  # introduce random NA values into a column
  X_reg_na[sample.int(nrow(X_reg_na), 20L), sample.int(ncol(X_reg_na) - 1L, 1L)] <- NA

  pred.reg <- predict(ens.reg, newdata = X_reg_na, na.action = na.pass)
  testthat::expect_identical(nrow(pred.reg), nrow(X_reg_na))

  pred.reg <- predict(ens.reg, newdata = X_reg_na)
  testthat::expect_false(nrow(pred.reg) != nrow(X_reg_na))
})

testthat::test_that("predict.caretStack works correctly if the multiclass excluded level is too high", {

  # Make sure predictions still work if the exlcuded level is too high
  meta_model <- caretStack(
    models_multiclass,
    method = "rpart",
    excluded_class_id = 4L
  )
  pred <- predict(meta_model, newdata = iris)
  testthat::expect_identical(nrow(pred), 150L)
  testthat::expect_identical(ncol(pred), 3L)
  all_finite <- function(x) all(is.finite(x))
  testthat::expect_true(all(vapply(pred, all_finite, logical(1L))))
})

testthat::test_that("caretStack handles different stacking algorithms", {
  for (x in list(list(models.reg, X.reg), list(models.class, X.class))) {
    model_list <- x[[1L]]
    test_data <- x[[2L]]

    stack_methods <- c("glm", "rf", "gbm", "glmnet")

    for (method in stack_methods) {
      if (method == "gbm") {
        stack <- caretStack(
          model_list,
          method = method,
          verbose = FALSE
        )
      } else {
        stack <- caretStack(
          model_list,
          method = method
        )
      }

      testthat::expect_s3_class(stack, "caretStack")
      testthat::expect_identical(stack$ens_model$method, method)

      predictions <- predict(stack, newdata = test_data)
      testthat::expect_identical(nrow(predictions), nrow(test_data))
    }
  }
})

testthat::test_that("caretStack handles missing data in new data", {
  models.class.subset <- models.class[c("rpart", "treebag")]

  stack <- caretStack(
    models.class.subset,
    method = "rpart"
  )

  test_data_with_na <- X.class
  test_data_with_na[1L:5L, 1L] <- NA

  pred <- predict(stack, newdata = test_data_with_na)
  testthat::expect_identical(nrow(pred), nrow(test_data_with_na))
})

testthat::test_that("caretStack handles different metrics", {
  metrics <- c("ROC", "Sens", "Spec")
  for (metric in metrics) {
    stack <- caretStack(
      models.class,
      method = "glm",
      metric = metric,
      trControl = caret::trainControl(
        method = "cv",
        number = 3L,
        classProbs = TRUE,
        summaryFunction = caret::twoClassSummary
      )
    )
    testthat::expect_s3_class(stack, "caretStack")
    testthat::expect_identical(stack$ens_model$metric, metric)
  }
})

testthat::test_that("caretStack handles upsampling data", {
  train_data <- iris

  imbalanced_data <- rbind(
    train_data[train_data$Species == "setosa", ],
    train_data[train_data$Species == "versicolor", ][1L:10L, ],
    train_data[train_data$Species == "virginica", ][1L:5L, ]
  )

  model_list <- caretList(
    x = imbalanced_data[, 1L:4L],
    y = imbalanced_data$Species,
    methodList = "rpart",
    trControl = caret::trainControl(
      method = "cv",
      number = 3L,
      classProbs = TRUE,
      sampling = "up",
      savePredictions = "final"
    )
  )

  stack <- caretStack(
    model_list,
    method = "rpart"
  )

  testthat::expect_s3_class(stack, "caretStack")
  pred <- predict(stack, newdata = imbalanced_data)
  testthat::expect_identical(nrow(pred), nrow(imbalanced_data))
})

testthat::test_that("caretStack handles custom preprocessing", {
  preprocess <- c("center", "scale", "pca")
  for (model_list in list(models.class, models.reg)) {
    stack <- caretStack(
      model_list,
      method = "glm",
      preProcess = preprocess
    )
    testthat::expect_s3_class(stack, "caretStack")
    testthat::expect_named(stack$ens_model$preProcess$method, c(preprocess, "ignore"))
  }
})

testthat::test_that("caretStack handles custom performance function", {
  custom_summary <- function(data, lev = NULL, model = NULL) {
    c(default = mean(data$obs == data$pred))
  }

  for (model_list in list(models.class, models.reg)) {
    stack <- caretStack(
      model_list,
      method = "glm",
      metric = "default",
      trControl = caret::trainControl(method = "cv", number = 3L, summaryFunction = custom_summary)
    )

    testthat::expect_s3_class(stack, "caretStack")
    testthat::expect_true("default" %in% names(stack$ens_model$results))
  }
})

testthat::test_that("predict.caretStack works if excluded_class_id is not set", {
  ens <- caretStack(models.class)
  ens[["excluded_class_id"]] <- NULL
  pred <- testthat::expect_warning(predict(ens, X.class), "No excluded_class_id set. Setting to 1L.")

  # Note that we don't exclude the class from the ensemble predictions, but merely from the preprocessing
  testthat::expect_s3_class(pred, "data.table") # caret returns data.frame
  testthat::expect_identical(nrow(pred), nrow(X.class))
  testthat::expect_identical(ncol(pred), 2L)
  testthat::expect_named(pred, c("No", "Yes"))
})

testthat::test_that("caretStack coerces lists to caretLists", {
  model_list <- models.reg
  class(model_list) <- "list"
  names(model_list) <- NULL
  ens <- testthat::expect_warning(
    caretStack(model_list),
    "Attempting to coerce all.models to a caretList."
  )
  testthat::expect_s3_class(ens, "caretStack")
  testthat::expect_s3_class(ens$models, "caretList")
  testthat::expect_named(ens$models, names(models.reg))
})

testthat::test_that("caretStack fails if new_X is NULL and newY is not and vice versa", {
  err <- "Both new_X and new_y must be NULL, or neither."
  testthat::expect_error(caretStack(models.reg, new_X = NULL, new_y = Y.reg), err)
  testthat::expect_error(caretStack(models.reg, new_X = X.reg, new_y = NULL), err)
})

testthat::test_that("caretStack works if both new_X and new_Y are supplied", {
  set.seed(42L)
  N <- 50L
  idx <- sample.int(nrow(X.reg), N)
  stack_class <- caretStack(
    models.class,
    new_X = X.class[idx, ],
    new_y = Y.class[idx],
    method = "rpart",
    # Need probs for stacked preds
    trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE)
  )
  stack_reg <- caretStack(
    models.reg,
    new_X = X.reg[idx, ],
    new_y = Y.reg[idx],
    method = "glm",
    # Need probs for stacked preds
    trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final", classProbs = FALSE)
  )

  testthat::expect_s3_class(stack_class, "caretStack")
  testthat::expect_s3_class(stack_reg, "caretStack")

  pred_class_stack <- predict(stack_class)
  stack_reg_stack <- predict(stack_reg)

  testthat::expect_s3_class(pred_class_stack, "data.table")
  testthat::expect_s3_class(stack_reg_stack, "data.table")

  testthat::expect_identical(nrow(pred_class_stack), N)
  testthat::expect_identical(nrow(stack_reg_stack), N)

  testthat::expect_identical(ncol(pred_class_stack), 2L)
  testthat::expect_identical(ncol(stack_reg_stack), 1L)

  pred_class <- predict(stack_class, new_X = X.class)
  pred_reg <- predict(stack_reg, new_X = X.reg)

  testthat::expect_s3_class(pred_class, "data.table")
  testthat::expect_s3_class(pred_reg, "data.table")

  testthat::expect_identical(nrow(pred_class), N)
  testthat::expect_identical(nrow(pred_reg), N)

  testthat::expect_identical(ncol(pred_class), 2L)
  testthat::expect_identical(ncol(pred_reg), 1L)
})

testthat::test_that("caretStack multiclass", {
  # Stacking with the excluded level should work
  model_stack <- caretStack(models_multiclass, method = "knn", excluded_class_id = 1L)

  # Check if we are actually excluding level 1 (setosa)
  classes <- levels(iris[, 5L])[-1L]
  models <- c("rpart", "glmnet")
  class_model_combinations <- expand.grid(classes, models)
  varImp_rownames <- apply(class_model_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  testthat::expect_identical(rownames(caret::varImp(model_stack$ens_model)$importance), varImp_rownames)

  # Stacking with too great of a level should work too. No error or warning.
  stack <- caretStack(models_multiclass, method = "knn", excluded_class_id = 4L)
  invisible(predict(stack, iris[, -5L]))
})

#############################################################################
testthat::context("varImp")
#############################################################################

testthat::test_that("varImp works in for class and reg in sample", {
  for (ens in list(ens.class, ens.reg)) {
    imp <- varImp(ens)
    expect_is(imp, "numeric")
    expect_named(imp, names(ens$models))
    expect_equal(sum(imp), 1.0, tolerance = 1e-6)
  }
})

testthat::test_that("varImp works in for class on new data", {
  imp <- varImp(ens.class, X.class)
  expect_is(imp, "numeric")
  expect_named(imp, names(ens.class$models))
  expect_equal(sum(imp), 1.0, tolerance = 1e-6)
})

testthat::test_that("varImp works in for reg on new data", {
  imp <- varImp(ens.reg, X.class)
  expect_is(imp, "numeric")
  expect_named(imp, names(ens.reg$models))
  expect_equal(sum(imp), 1.0, tolerance = 1e-6)
})

#############################################################################
testthat::context("wtd.sd")
#############################################################################

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
