library(testthat)
library(caret)

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

context("Does stacking and prediction work?")

test_that("We can stack regression models", {
  set.seed(96367L)
  ens.reg <- caretStack(
    models.reg,
    method = "lm", preProcess = "pca",
    trControl = trainControl(number = 2L, allowParallel = FALSE)
  )
  expect_that(ens.reg, is_a("caretStack"))
  expect_is(summary(ens.reg), "summary.lm")
  invisible(capture.output(print(ens.reg)))
  pred.reg <- predict(ens.reg, newdata = X.reg)
  expect_type(pred.reg, "double")
  expect_length(pred.reg, 150L)
})

test_that("We can stack classification models", {
  set.seed(42L)
  ens.class <- caretStack(
    models.class,
    method = "glm",
    trControl = trainControl(number = 2L, allowParallel = FALSE)
  )
  expect_that(ens.class, is_a("caretStack"))
  expect_is(summary(ens.class), "summary.glm")
  invisible(capture.output(print(ens.class)))
  pred.class <- predict(ens.class, X.class, type = "prob")
  expect_true(all(sapply(pred.class, is.numeric)))
  expect_identical(nrow(pred.class), 150L)
  raw.class <- predict(ens.class, X.class, type = "raw")
  expect_s3_class(raw.class, "factor")
  expect_length(raw.class, 150L)
})

test_that("caretStack plots", {
  test_plot_file <- "caretEnsemble_test_plots.png"
  ens.reg <- caretStack(
    models.reg,
    method = "gbm", tuneLength = 2L, verbose = FALSE,
    trControl = trainControl(number = 2L, allowParallel = FALSE)
  )
  png(filename = test_plot_file)
  plot(ens.reg)
  dotplot(ens.reg, metric = "RMSE")
  dev.off()
  unlink(test_plot_file)
  expect_is(ens.reg, "caretStack")
})

context("Prediction errors for caretStack work as expected")

test_that("Failure to calculate se occurs gracefully", {
  ens.class <- caretStack(
    models.class,
    method = "glm",
    trControl = trainControl(number = 2L, allowParallel = FALSE)
  )

  predict(ens.class, X.class, type = "raw", se = TRUE)
  expect_is(predict(ens.class, X.class, type = "raw"), "factor")
  expect_is(predict(ens.class, X.class, type = "raw", se = TRUE), "factor")
  expect_identical(
    predict(ens.class, X.class, type = "raw", se = TRUE),
    predict(ens.class, X.class, type = "raw")
  )
  ens.reg <- caretStack(
    models.reg,
    method = "lm", preProcess = "pca",
    trControl = trainControl(number = 2L, allowParallel = FALSE)
  )
  expect_is(predict(ens.reg, X.reg, se = TRUE), "data.frame")

  expect_is(predict(ens.class, X.class, type = "prob", se = TRUE), "data.frame")
  expect_is(
    predict(
      ens.class, X.class,
      type = "prob", se = TRUE, return_weights = TRUE
    ), "data.frame"
  )
  expect_identical(
    colnames(predict(ens.class, X.class, type = "prob", se = TRUE)),
    c("No", "Yes")
  )
  expect_false(
    identical(
      predict(ens.class, X.class, type = "raw", return_weights = TRUE),
      predict(ens.class, X.class, type = "raw", return_weights = FALSE)
    )
  )
  expect_false(
    identical(
      predict(ens.class, X.class, type = "prob", se = TRUE),
      predict(ens.class, X.class, type = "prob", se = TRUE, return_weights = TRUE)
    )
  )
  expect_identical(
    predict(ens.class, X.class, type = "prob", se = TRUE, level = 0.8),
    predict(ens.class, X.class, type = "prob", se = TRUE, return_weights = FALSE)
  )
  expect_true(
    identical(
      predict(ens.class, X.class, type = "prob", level = 0.8),
      predict(ens.class, X.class, type = "prob", return_weights = FALSE)
    )
  )
})

test_that("Test na.action pass through", {
  set.seed(1337L)

  # drop the first model because it does not support na.pass
  ens.reg <- caretStack(models.reg[2L:3L], method = "lm")

  X_reg_na <- X.reg
  # introduce random NA values into a column
  X_reg_na[sample.int(nrow(X_reg_na), 20L), sample.int(ncol(X_reg_na) - 1L, 1L)] <- NA

  pred.reg <- predict(ens.reg, newdata = X_reg_na, na.action = na.pass)
  expect_length(pred.reg, nrow(X_reg_na))

  pred.reg <- predict(ens.reg, newdata = X_reg_na)
  expect_false(length(pred.reg) != nrow(X_reg_na))
})

test_that("is.caretStack correctly identifies caretStack objects", {
  expect_true(is.caretStack(structure(list(), class = "caretStack")))
  expect_false(is.caretStack(list()))
})

test_that("predict.caretStack works correctly if the multiclass excluded level is too high", {
  data(iris)

  # Create a caretList
  model_list <- caretList(
    Species ~ .,
    data = iris,
    trControl = trainControl(
      method = "cv", classProbs = TRUE, savePredictions = "final",
      index = createResample(iris$Species)
    ),
    methodList = c("rpart", "rf")
  )

  # Make sure predictions still work if the exlcuded level is too high
  meta_model <- caretStack(
    model_list,
    method = "rpart",
    trControl = trainControl(method = "cv"),
    excluded_class_id = 4L
  )
  pred <- predict(meta_model, newdata = iris, type = "prob")
  expect_equal(nrow(pred), 150L)
  expect_equal(ncol(pred), 3L)
  expect_true(all(sapply(pred, is.finite)))
})

context("caretStack edge cases")

test_that("caretStack handles different stacking algorithms", {
  for (x in list(list(models.reg, X.reg), list(models.class, X.class))) {
    model_list <- x[[1L]]
    test_data <- x[[2L]]

    stack_methods <- c("glm", "rf", "gbm", "glmnet")

    for (method in stack_methods) {
      if (method == "gbm") {
        stack <- caretStack(
          model_list,
          method = method,
          trControl = trainControl(method = "cv", number = 3L),
          verbose = FALSE
        )
      } else {
        stack <- caretStack(
          model_list,
          method = method,
          trControl = trainControl(method = "cv", number = 3L)
        )
      }

      expect_s3_class(stack, "caretStack")
      expect_equal(stack$ens_model$method, method)

      predictions <- predict(stack, newdata = test_data)
      expect_length(predictions, nrow(test_data))
    }
  }
})

test_that("caretStack handles missing data in new data", {
  models.class.subset <- models.class[c("rpart", "treebag")]

  stack <- caretStack(
    models.class.subset,
    method = "rpart",
    trControl = trainControl(method = "cv", number = 3L)
  )

  test_data_with_na <- X.class
  test_data_with_na[1L:5L, 1L] <- NA

  pred <- predict(stack, newdata = test_data_with_na)
  expect_length(pred, nrow(test_data_with_na))
})

test_that("caretStack handles different metrics", {
  metrics <- c("ROC", "Sens", "Spec")
  for (metric in metrics) {
    stack <- caretStack(
      models.class,
      method = "glm",
      metric = metric,
      trControl = trainControl(
        method = "cv", number = 3L, classProbs = TRUE,
        summaryFunction = twoClassSummary
      )
    )
    expect_s3_class(stack, "caretStack")
    expect_equal(stack$ens_model$metric, metric)
  }
})

test_that("caretStack handles imbalanced data", {
  data(iris)
  train_data <- iris

  imbalanced_data <- rbind(
    train_data[train_data$Species == "setosa", ],
    train_data[train_data$Species == "versicolor", ][1L:10L, ],
    train_data[train_data$Species == "virginica", ][1L:5L, ]
  )

  expect_warning({
    model_list <- caretList(
      x = imbalanced_data[, 1L:4L],
      y = imbalanced_data$Species,
      methodList = "rpart",
      trControl = trainControl(
        method = "cv",
        number = 3L,
        classProbs = TRUE,
        sampling = "up",
        savePredictions = "final"
      )
    )
  })

  stack <- caretStack(
    model_list,
    method = "rpart",
    metric = "Kappa",
    trControl = trainControl(method = "cv", number = 3L)
  )

  expect_s3_class(stack, "caretStack")
  pred <- predict(stack, newdata = imbalanced_data)
  expect_length(pred, nrow(imbalanced_data))
})

test_that("caretStack handles custom preprocessing", {
  preprocess <- c("center", "scale", "pca")
  for (model_list in list(models.class, models.reg)) {
    stack <- caretStack(
      model_list,
      method = "glm",
      preProcess = preprocess,
      trControl = trainControl(method = "cv", number = 3L)
    )
    expect_s3_class(stack, "caretStack")
    expect_named(stack$ens_model$preProcess$method, c(preprocess, "ignore"))
  }
})

test_that("caretStack handles custom performance function", {
  custom_summary <- function(data, lev = NULL, model = NULL) {
    c(default = mean(data$obs == data$pred))
  }

  for (model_list in list(models.class, models.reg)) {
    stack <- caretStack(
      model_list,
      method = "glm",
      metric = "default",
      trControl = trainControl(method = "cv", number = 3L, summaryFunction = custom_summary)
    )

    expect_s3_class(stack, "caretStack")
    expect_true("default" %in% names(stack$ens_model$results))
  }
})

test_that("predict.caretStack works if excluded_class_id is not set", {
  ens <- caretStack(models.class)
  ens[["excluded_class_id"]] <- NULL
  expect_warning(pred <- predict(ens, X.class, type = "prob"), "No excluded_class_id set.  Setting to 1L.")

  # Note that we don't exclude the class from the ensemble predictions, but merely from the preprocessing
  expect_is(pred, "data.frame") # caret returns data.frame
  expect_equal(nrow(pred), nrow(X.class))
  expect_equal(ncol(pred), 2L)
  expect_named(pred, c("No", "Yes"))
})
