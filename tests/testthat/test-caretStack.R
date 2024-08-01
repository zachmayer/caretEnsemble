data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

testthat::context("Does stacking and prediction work?")

testthat::test_that("We can stack regression models", {
  set.seed(96367L)
  ens.reg <- caretStack(
    models.reg,
    method = "lm", preProcess = "pca"
  )
  testthat::expect_s3_class(ens.reg, "caretStack")
  testthat::expect_is(summary(ens.reg), "summary.lm")
  invisible(capture.output(print(ens.reg)))
  pred.reg <- predict(ens.reg, newdata = X.reg)
  testthat::expect_s3_class(pred.reg, "data.table")
  testthat::expect_identical(nrow(pred.reg), 150L)
})

testthat::test_that("We can stack classification models", {
  set.seed(42L)
  ens.class <- caretStack(
    models.class,
    method = "glm"
  )
  testthat::expect_that(ens.class, is_a("caretStack"))
  testthat::expect_is(summary(ens.class), "summary.glm")
  invisible(capture.output(print(ens.class)))
  pred.class <- predict(ens.class, X.class)
  testthat::expect_true(all(sapply(pred.class, is.numeric)))
  testthat::expect_identical(nrow(pred.class), 150L)
  raw.class <- predict(ens.class, X.class)
  testthat::expect_s3_class(raw.class, "data.table")
  testthat::expect_identical(nrow(raw.class), 150L)
})

testthat::test_that("caretStack plots", {
  test_plot_file <- "caretEnsemble_test_plots.png"
  ens.reg <- caretStack(
    models.reg,
    method = "gbm", tuneLength = 2L, verbose = FALSE
  )
  png(filename = test_plot_file)
  plot(ens.reg)
  lattice::dotplot(ens.reg, metric = "RMSE")
  dev.off()
  unlink(test_plot_file)
  testthat::expect_s3_class(ens.reg, "caretStack")
})

testthat::context("Prediction errors for caretStack work as expected")

testthat::test_that("Failure to calculate se occurs gracefully", {
  ens.class <- caretStack(
    models.class,
    method = "glm", preProcess = "pca"
  )

  w <- "Cannot calculate standard errors due to the preprocessing used in train"
  testthat::expect_warning(predict(ens.class, X.class, se = TRUE), w)
  testthat::expect_s3_class(predict(ens.class, X.class), "data.table")
  testthat::expect_warning(testthat::expect_s3_class(predict(ens.class, X.class, se = TRUE), "data.table"), w)
  testthat::expect_warning(testthat::expect_equivalent(
    predict(ens.class, X.class, return_weights = TRUE),
    predict(ens.class, X.class)
  ), w)
  ens.reg <- caretStack(
    models.reg,
    method = "glm", preProcess = "pca"
  )
  testthat::expect_warning(testthat::expect_s3_class(predict(ens.reg, X.reg, se = TRUE), "data.table"), w)

  testthat::expect_warning(testthat::expect_s3_class(predict(ens.class, X.class, se = TRUE), "data.table"), w)
  testthat::expect_warning(
    testthat::expect_s3_class(
      predict(ens.class, X.class, se = TRUE, return_weights = TRUE),
      "data.table"
    ), w
  )

  testthat::expect_warning(
    testthat::expect_named(
      predict(ens.class, X.class, se = TRUE, excluded_class_id = 0L),
      c("No", "Yes")
    ), w
  )
  testthat::expect_warning(
    testthat::expect_false(
      identical(
        predict(ens.class, X.class, return_weights = TRUE),
        predict(ens.class, X.class, return_weights = FALSE)
      )
    ), w
  )
  testthat::expect_warning(testthat::expect_false(
    identical(
      predict(ens.class, X.class, se = TRUE),
      predict(ens.class, X.class, se = TRUE, return_weights = TRUE)
    )
  ), w)
  testthat::expect_warning(testthat::expect_equivalent(
    predict(ens.class, X.class, se = TRUE, level = 0.8, return_weights = TRUE),
    predict(ens.class, X.class, se = TRUE, level = 0.8, return_weights = FALSE)
  ), w)
  testthat::expect_warning(testthat::expect_equivalent(
    predict(ens.class, X.class, level = 0.8, return_weights = TRUE),
    predict(ens.class, X.class, level = 0.8, return_weights = FALSE)
  ), w)
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

testthat::test_that("is.caretStack correctly identifies caretStack objects", {
  testthat::expect_true(is.caretStack(structure(list(), class = "caretStack")))
  testthat::expect_false(is.caretStack(list()))
})

testthat::test_that("predict.caretStack works correctly if the multiclass excluded level is too high", {
  data(iris)

  # Create a caretList
  model_list <- caretList(
    Species ~ .,
    data = iris,
    methodList = c("rpart", "rf")
  )

  # Make sure predictions still work if the exlcuded level is too high
  meta_model <- caretStack(
    model_list,
    method = "rpart",
    excluded_class_id = 4L
  )
  pred <- predict(meta_model, newdata = iris)
  testthat::expect_equal(nrow(pred), 150L)
  testthat::expect_equal(ncol(pred), 3L)
  testthat::expect_true(all(sapply(pred, is.finite)))
})

testthat::context("caretStack edge cases")

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
      testthat::expect_equal(stack$ens_model$method, method)

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
    testthat::expect_equal(stack$ens_model$metric, metric)
  }
})

testthat::test_that("caretStack handles upsampling data", {
  data(iris)
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
  testthat::expect_equal(nrow(pred), nrow(X.class))
  testthat::expect_equal(ncol(pred), 2L)
  testthat::expect_named(pred, c("No", "Yes"))
})

testthat::context("Edge cases")

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

  testthat::expect_equal(nrow(pred_class_stack), N)
  testthat::expect_equal(nrow(stack_reg_stack), N)

  testthat::expect_equal(ncol(pred_class_stack), 2L)
  testthat::expect_equal(ncol(stack_reg_stack), 1L)

  pred_class <- predict(stack_class, new_X = X.class)
  pred_reg <- predict(stack_reg, new_X = X.reg)

  testthat::expect_s3_class(pred_class, "data.table")
  testthat::expect_s3_class(pred_reg, "data.table")

  testthat::expect_equal(nrow(pred_class), N)
  testthat::expect_equal(nrow(pred_reg), N)

  testthat::expect_equal(ncol(pred_class), 2L)
  testthat::expect_equal(ncol(pred_reg), 1L)
})
