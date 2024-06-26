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
  suppressWarnings(pred.reg <- predict(ens.reg, newdata = X.reg))
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
  suppressWarnings(pred.class <- predict(ens.class, X.class, type = "prob"))
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class) == 150)
  suppressWarnings(raw.class <- predict(ens.class, X.class, type = "raw"))
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

  suppressWarnings(predict(ens.class, X.class, type = "raw", se = TRUE))
  suppressWarnings(expect_is(predict(ens.class, X.class, type = "raw"), "factor"))
  suppressWarnings(expect_is(predict(ens.class, X.class, type = "raw", se = TRUE), "factor"))
  suppressWarnings({
    expect_identical(
      predict(ens.class, X.class, type = "raw", se = TRUE),
      predict(ens.class, X.class, type = "raw")
    )
  })
  ens.reg <- caretStack(
    models.reg,
    method = "lm", preProcess = "pca",
    trControl = trainControl(number = 2, allowParallel = FALSE)
  )
  suppressWarnings(pred <- predict(ens.reg, X.reg, se = TRUE))
  suppressWarnings(expect_is(predict(ens.reg, X.reg, se = TRUE), "data.frame"))

  suppressWarnings(expect_is(predict(ens.class, X.class, type = "prob", se = TRUE), "data.frame"))
  suppressWarnings(
    expect_is(
      predict(
        ens.class, X.class,
        type = "prob", se = TRUE, return_weights = TRUE
      ), "data.frame"
    )
  )
  suppressWarnings(
    expect_identical(
      colnames(predict(ens.class, X.class, type = "prob", se = TRUE)),
      c("fit", "lwr", "upr")
    )
  )
  suppressWarnings(
    expect_false(
      identical(
        predict(ens.class, X.class, type = "raw", return_weights = TRUE),
        predict(ens.class, X.class, type = "raw", return_weights = FALSE)
      )
    )
  )
  suppressWarnings(
    expect_false(
      identical(
        predict(ens.class, X.class, type = "prob", se = TRUE, level = 0.8),
        predict(ens.class, X.class, type = "prob", se = TRUE, return_weights = FALSE)
      )
    )
  )
  suppressWarnings(
    expect_true(
      identical(
        predict(ens.class, X.class, type = "prob", level = 0.8),
        predict(ens.class, X.class, type = "prob", return_weights = FALSE)
      )
    )
  )
})

test_that("Test na.action pass through", {
  set.seed(1337)

  # drop the first model because it does not support na.pass
  ens.reg <- caretStack(models.reg[2:3], method = "lm")

  X_reg_na <- X.reg
  # introduce random NA values into a column
  X_reg_na[sample(1:nrow(X_reg_na), 20), sample(1:ncol(X_reg_na) - 1, 1)] <- NA

  suppressWarnings(pred.reg <- predict(ens.reg, newdata = X_reg_na, na.action = na.pass))
  expect_length(pred.reg, nrow(X_reg_na))

  suppressWarnings(pred.reg <- predict(ens.reg, newdata = X_reg_na))
  expect_false(length(pred.reg) != nrow(X_reg_na))
})
