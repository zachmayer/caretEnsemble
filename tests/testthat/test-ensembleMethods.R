# Are tests failing here?
# UPDATE THE FIXTURES!
# make update-test-fixtures

testthat::context("Does variable importance work?")

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

testthat::test_that("We can get variable importance in ensembles", {
  set.seed(2239L)
  ens.class <- caretEnsemble(models.class, trControl = caret::trainControl(method = "none"))
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(method = "none"))
  testthat::expect_s3_class(caret::varImp(ens.class), "data.table")
  testthat::expect_s3_class(caret::varImp(ens.class, scale = TRUE), "data.table")
  testthat::expect_s3_class(caret::varImp(ens.reg), "data.table")
  testthat::expect_s3_class(caret::varImp(ens.reg, scale = TRUE), "data.table")
})

testthat::test_that("caret::varImp works for caretEnsembles", {
  set.seed(2239L)
  for (models in list(models.class, models.reg)) {
    ens <- caretEnsemble(models, trControl = caret::trainControl(method = "none"))
    expected_names <- c("var", "overall", names(ens$models))
    expected_var_names <- c(
      "Intercept",
      "Speciesversicolor",
      "Speciesvirginica",
      "Petal.Width",
      "Sepal.Width",
      "Petal.Length"
    )
    for (s in c(TRUE, FALSE)) {
      i <- caret::varImp(ens, scale = s)
      testthat::expect_s3_class(i, "data.table")
      testthat::expect_named(i, expected_names)
      testthat::expect_identical(sort(i[["var"]]), sort(expected_var_names))
    }
  }
})

testthat::test_that("We get the right dimensions back", {
  set.seed(2239L)
  ens.class <- caretEnsemble(models.class, trControl = caret::trainControl(method = "none"))
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(method = "none"))
  testthat::expect_identical(ncol(caret::varImp(ens.class)), 6L)
  testthat::expect_identical(ncol(caret::varImp(ens.reg)), 6L)
  testthat::expect_identical(nrow(caret::varImp(ens.class)), 6L)
  testthat::expect_identical(nrow(caret::varImp(ens.reg)), 6L)
})

testthat::context("Do metric extraction functions work as expected")

testthat::test_that("Metric is used correctly", {
  set.seed(2239L)

  # Make ensemble
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = caret::trainControl(
      number = 2L,
      summaryFunction = caret::twoClassSummary,
      classProbs = TRUE
    )
  )

  # Make ensemble
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(number = 2L))

  # Get an incorrect metric
  err <- "metric %in% names(x$results) is not TRUE"
  testthat::expect_error(getMetric(ens.class$models[[3L]], "RMSE"), err, fixed = TRUE)
  testthat::expect_error(getMetric(ens.reg$models[[2L]], "ROC"), err, fixed = TRUE)

  # Correct metric
  testthat::expect_equal(getMetric(ens.class$models[[1L]], "ROC"), 0.9293333, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[2L]], "ROC"), 0.9406667, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[3L]], "ROC"), 0.8826667, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[4L]], "ROC"), 0.9153333, tol = 0.1)

  # Correct metric
  testthat::expect_equal(getMetric(ens.reg$models[[1L]], "RMSE"), 0.3146584, tol = 0.1)
  testthat::expect_equal(getMetric(ens.reg$models[[2L]], "RMSE"), 0.308, tol = 0.1)
  testthat::expect_equal(getMetric(ens.reg$models[[3L]], "RMSE"), 0.444, tol = 0.1)

  # Correct metric
  testthat::expect_equal(getMetric(ens.class$models[[1L]], "ROC", return_sd = TRUE), 0.05897897, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[2L]], "ROC", return_sd = TRUE), 0.05196865, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[3L]], "ROC", return_sd = TRUE), 0.05985304, tol = 0.1)
  testthat::expect_equal(getMetric(ens.class$models[[4L]], "ROC", return_sd = TRUE), 0.07554248, tol = 0.1)

  # Correct metric
  testthat::expect_equal(getMetric(ens.reg$models[[1L]], "RMSE", return_sd = TRUE), 0.05839238, tol = 0.1)
  testthat::expect_equal(getMetric(ens.reg$models[[2L]], "RMSE", return_sd = TRUE), 0.06043732, tol = 0.1)
  testthat::expect_equal(getMetric(ens.reg$models[[3L]], "RMSE", return_sd = TRUE), 0.06942881, tol = 0.1)
})

testthat::context("Testing caretEnsemble generics")

testthat::test_that("No errors are thrown by a generics for ensembles", {
  test_plot_file <- "caretEnsemble_test_plots.png"
  png(test_plot_file)
  set.seed(2239L)
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = caret::trainControl(
      number = 2L,
      summaryFunction = caret::twoClassSummary,
      classProbs = TRUE,
      savePredictions = TRUE
    )
  )
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(number = 2L, savePredictions = TRUE))
  testthat::expect_output(summary(ens.class), "ROC")
  testthat::expect_output(summary(ens.reg), "RMSE")

  testthat::expect_is(plot(ens.class), "ggplot")
  testthat::expect_is(plot(ens.reg), "ggplot")

  tp <- plot(ens.class)
  tp2 <- plot(ens.reg)
  testthat::expect_identical(nrow(tp$data), 4L)
  testthat::expect_identical(nrow(tp2$data), 4L)

  testthat::expect_named(ens.class$models, tp$data$model_name)
  testthat::expect_named(ens.reg$models, tp2$data$model_name)

  autoplot(ens.class)
  autoplot(ens.reg)
  autoplot(ens.class, xvars = c("Petal.Length", "Petal.Width"))
  autoplot(ens.reg, xvars = c("Petal.Length", "Petal.Width"))
  testthat::expect_error(autoplot(ens.reg$models[[1L]]), "Objects of class (.*?) are not supported by autoplot")

  dev.off()
  testthat::expect_true(file.exists(test_plot_file))
  unlink(test_plot_file)
})


testthat::context("Are ensembles construct accurately")

testthat::test_that("Do model results in caretEnsemble match component models - classification", {
  set.seed(2239L)
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = caret::trainControl(
      number = 2L, summaryFunction = caret::twoClassSummary, classProbs = TRUE
    )
  )
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(number = 2L))
  modres1 <- extractModelMetrics(ens.class)
  modres2 <- extractModelMetrics(ens.reg)
  testthat::expect_s3_class(modres2, "data.table")
  testthat::expect_named(models.reg, modres2$model_name)
})

testthat::test_that("Do model results in caretEnsemble match component models - regression", {
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(method = "none"))
  ens.class <- caretEnsemble(models.class, trControl = caret::trainControl(method = "none"))
  newDat <- ens.class$models[[3L]]$trainingData
  newDat[2L, 2L] <- NA
  newDat[3L, 3L] <- NA
  newDat[4L, 4L] <- NA
  newDat <- newDat[1L:10L, ]

  # These yield errors on NAs because predict.randomForest can't handle NAs in new data
  err <- "missing values in newdata"
  testthat::expect_error(predict(ens.class, newdata = newDat, return_weights = TRUE, se = FALSE), err)
  testthat::expect_error(predict(ens.reg, newdata = newDat, return_weights = TRUE, se = TRUE), err)
  testthat::expect_error(predict(ens.reg, newdata = newDat, return_weights = FALSE, se = FALSE), err)
  testthat::expect_error(predict(ens.reg, newdata = newDat, return_weights = FALSE, se = TRUE), err)
})

testthat::test_that("Prediction options are respected in regression", {
  ens.reg <- caretEnsemble(models.reg, trControl = caret::trainControl(method = "none"))
  tests <- expand.grid(se = 0L:1L, return_weights = 0L:1L)
  tests <- data.table::as.data.table(lapply(tests, as.logical))
  for (i in seq_len(nrow(tests))) {
    p <- predict(
      ens.reg,
      newdata = X.reg,
      se = tests[i, ][["se"]],
      return_weights = tests[i, ][["return_weights"]]
    )

    testthat::expect_s3_class(p, "data.table")

    if (tests[i, ][["return_weights"]]) {
      testthat::expect_is(attr(p, which = "weights"), "numeric")
    } else {
      testthat::expect_null(attr(p, which = "weights"))
    }
  }
})

testthat::test_that("Prediction options are respected in Classification", {
  ens.class <- caretEnsemble(models.class, trControl = caret::trainControl(method = "none"))
  tests <- expand.grid(se = 0L:1L, return_weights = 0L:1L)
  tests <- data.table::as.data.table(lapply(tests, as.logical))
  for (i in seq_len(nrow(tests))) {
    p <- predict(
      ens.class,
      newdata = X.class,
      se = tests[i, ][["se"]],
      return_weights = tests[i, ][["return_weights"]]
    )

    testthat::expect_s3_class(p, "data.table")

    if (tests[i, ][["return_weights"]]) {
      testthat::expect_is(unlist(attr(p, which = "weights")), "numeric")
    } else {
      testthat::expect_null(attr(p, which = "weights"))
    }
  }
})
