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
  set.seed(96367)
  ens.reg <- caretStack(
    models.reg,
    method = "lm", preProcess = "pca",
    trControl = trainControl(number = 2, allowParallel = FALSE)
  )
  expect_that(ens.reg, is_a("caretStack"))
  expect_is(summary(ens.reg), "summary.lm")
  invisible(capture.output(print(ens.reg)))
  pred.reg <- predict(ens.reg, newdata = X.reg) # In the past this expected a warning, I do not know why. I think is caused by the used method (lm)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg) == 150)
})

test_that("We can stack classification models", {
  set.seed(42)
  ens.class <- caretStack(
    models.class,
    method = "glm",
    trControl = trainControl(number = 2, allowParallel = FALSE)
  )
  expect_that(ens.class, is_a("caretStack"))
  expect_is(summary(ens.class), "summary.glm")
  invisible(capture.output(print(ens.class)))
  pred.class <- predict(ens.class, X.class, type = "prob")
  expect_true(all(sapply(pred.class, is.numeric)))
  expect_true(nrow(pred.class) == 150)
  raw.class <- predict(ens.class, X.class, type = "raw")
  expect_true(is.factor(raw.class))
  expect_true(length(raw.class) == 150)
})

test_that("caretStack plots", {
  test_plot_file <- "caretEnsemble_test_plots.png"
  ens.reg <- caretStack(
    models.reg,
    method = "gbm", tuneLength = 2, verbose = FALSE,
    trControl = trainControl(number = 2, allowParallel = FALSE)
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
    trControl = trainControl(number = 2, allowParallel = FALSE)
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
    trControl = trainControl(number = 2, allowParallel = FALSE)
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
  set.seed(1337)

  # drop the first model because it does not support na.pass
  ens.reg <- caretStack(models.reg[2:3], method = "lm")

  X_reg_na <- X.reg
  # introduce random NA values into a column
  X_reg_na[sample(seq_len(nrow(X_reg_na)), 20), sample(seq_len(ncol(X_reg_na) - 1), 1)] <- NA

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
  expect_warning({
      meta_model <- caretStack(
        model_list,
        method = "rpart",
        trControl = trainControl(method = "cv"),
        excluded_class_id = 4L
      )
    },
    "Value for caret.ensemble.multiclass.excluded.level is outside the range between 1 and the number of classes. Using all classes to train meta-model."
  )
  expect_warning({
      pred <- predict(meta_model, newdata = iris, type = "prob")
    },
    "Value for caret.ensemble.multiclass.excluded.level is outside the range between 1 and the number of classes. Returning all classes."
  )
  expect_equal(nrow(pred), 150)
  expect_equal(ncol(pred), 3)
  expect_true(all(sapply(pred, is.finite)))
})

context("caretStack edge cases")

test_that("caretStack handles different stacking algorithms", {
  for (x in list(list(models.reg, X.reg), list(models.class, X.class))) {
    model_list <- x[[1]]
    test_data <- x[[2]]

    stack_methods <- c("glm", "rf", "gbm", "glmnet")

    for (method in stack_methods) {
      if (method == "gbm") {
        stack <- caretStack(
          model_list,
          method = method,
          trControl = trainControl(method = "cv", number = 3),
          verbose = FALSE
        )
      } else {
        stack <- caretStack(
          model_list,
          method = method,
          trControl = trainControl(method = "cv", number = 3)
        )
      }

      expect_s3_class(stack, "caretStack")
      expect_equal(stack$ens_model$method, method)

      predictions <- predict(stack, newdata = test_data)
      expect_equal(length(predictions), nrow(test_data))
    }
  }
})

test_that("caretStack handles missing data in new data", {
  models.class.subset <- models.class[c("rpart", "treebag")]

  stack <- caretStack(
    models.class.subset,
    method = "rpart",
    trControl = trainControl(method = "cv", number = 3)
  )

  test_data_with_na <- X.class
  test_data_with_na[1:5, 1] <- NA

  pred <- predict(stack, newdata = test_data_with_na)
  expect_equal(length(pred), nrow(test_data_with_na))
})

test_that("caretStack handles different metrics", {
  metrics <- c("ROC", "Sens", "Spec")
  for (metric in metrics) {
    stack <- caretStack(
      models.class,
      method = "glm",
      metric = metric,
      trControl = trainControl(
        method = "cv", number = 3, classProbs = TRUE,
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
    train_data[train_data$Species == "versicolor", ][1:10, ],
    train_data[train_data$Species == "virginica", ][1:5, ]
  )

  expect_warning({
    model_list <- caretList(
      x = imbalanced_data[, 1:4],
      y = imbalanced_data$Species,
      methodList = "rpart",
      trControl = trainControl(
        method = "cv",
        number = 3,
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
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_s3_class(stack, "caretStack")
  pred <- predict(stack, newdata = imbalanced_data)
  expect_equal(length(pred), nrow(imbalanced_data))
})

test_that("caretStack handles custom preprocessing", {
  preprocess <- c("center", "scale", "pca")
  for (model_list in list(models.class, models.reg)) {
    stack <- caretStack(
      model_list,
      method = "glm",
      preProcess = preprocess,
      trControl = trainControl(method = "cv", number = 3)
    )
    expect_s3_class(stack, "caretStack")
    expect_equal(names(stack$ens_model$preProcess$method), c(preprocess, "ignore"))
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
      trControl = trainControl(method = "cv", number = 3, summaryFunction = custom_summary)
    )

    expect_s3_class(stack, "caretStack")
    expect_true("default" %in% names(stack$ens_model$results))
  }
})
