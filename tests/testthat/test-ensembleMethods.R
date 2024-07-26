# Are tests failing here?
# UPDATE THE FIXTURES!
# make update-test-fixtures

context("Does variable importance work?")
library(caret)

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

test_that("We can get variable importance in ensembles", {
  set.seed(2239L)
  ens.class <- caretEnsemble(models.class, trControl = trainControl(method = "none"))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2L:4L]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl = trainControl(method = "none"))
  expect_is(varImp(ens.class), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg), "data.frame")
  expect_is(varImp(ens.reg, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("varImp works for caretEnsembles", {
  set.seed(2239L)
  for (models in list(models.class, models.reg)) {
    ens <- caretEnsemble(models, trControl = trainControl(method = "none"))
    expected_names <- c("overall", names(ens$models))
    expected_row_names <- c(
      "Intercept",
      "Speciesversicolor",
      "Speciesvirginica",
      "Petal.Width",
      "Sepal.Width",
      "Petal.Length"
    )
    for (s in c(TRUE, FALSE)) {
      for (w in c(TRUE, FALSE)) {
        i <- varImp(ens, scale = s, weight = w)
        expect_is(i, "data.frame")
        expect_named(i, expected_names)
        expect_equal(sort(row.names(i)), sort(expected_row_names))
      }
    }
  }
})

test_that("We get the right dimensions back", {
  ncol1 <- 5L
  ncol2 <- 4L
  nrow1 <- 6L
  nrow2 <- 6L
  set.seed(2239L)
  ens.class <- caretEnsemble(models.class, trControl = trainControl(method = "none"))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2L:4L]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl = trainControl(method = "none"))
  expect_equal(ncol(varImp(ens.class)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = FALSE)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = TRUE)), ncol1)
  expect_equal(ncol(varImp(ens.reg)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = FALSE)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = TRUE)), ncol2)
  expect_equal(nrow(varImp(ens.class)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = FALSE)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = TRUE)), nrow1)
  expect_equal(nrow(varImp(ens.reg)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = FALSE)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = TRUE)), nrow2)
})

context("Do metric extraction functions work as expected")

test_that("Metric is used correctly", {
  set.seed(2239L)

  # Make ensemble
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = trainControl(
      number = 2L,
      summaryFunction = twoClassSummary,
      classProbs = TRUE
    )
  )

  # Make ensemble
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2L:4L]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl = trainControl(number = 2L))

  # Get an incorrect metric
  expect_error(getMetric(ens.class$models[[3L]], metric = "RMSE"))
  expect_error(getMetric(ens.reg$models[[2L]], metric = "ROC"))

  # Correct metric
  expect_equal(getMetric(ens.class$models[[1L]], metric = "ROC"), 0.9293333, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[2L]], metric = "ROC"), 0.9406667, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[3L]], metric = "ROC"), 0.8826667, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[4L]], metric = "ROC"), 0.9153333, tol = 0.1)

  # Correct metric
  expect_equal(getMetric(ens.reg$models[[1L]], metric = "RMSE"), 0.3146584, tol = 0.1)
  expect_equal(getMetric(ens.reg$models[[2L]], metric = "RMSE"), 0.439482, tol = 0.1)
  expect_equal(getMetric(ens.reg$models[[3L]], metric = "RMSE"), 0.3361409, tol = 0.1)

  # Correct metric
  expect_equal(getMetric(ens.class$models[[1L]], metric = "ROC", return_sd = TRUE), 0.05897897, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[2L]], metric = "ROC", return_sd = TRUE), 0.05196865, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[3L]], metric = "ROC", return_sd = TRUE), 0.05985304, tol = 0.1)
  expect_equal(getMetric(ens.class$models[[4L]], metric = "ROC", return_sd = TRUE), 0.07554248, tol = 0.1)

  # Correct metric
  expect_equal(getMetric(ens.reg$models[[1L]], metric = "RMSE", return_sd = TRUE), 0.05839238, tol = 0.1)
  expect_equal(getMetric(ens.reg$models[[2L]], metric = "RMSE", return_sd = TRUE), 0.06043732, tol = 0.1)
  expect_equal(getMetric(ens.reg$models[[3L]], metric = "RMSE", return_sd = TRUE), 0.06942881, tol = 0.1)
})

context("Testing caretEnsemble generics")

test_that("No errors are thrown by a generics for ensembles", {
  set.seed(2239L)
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = trainControl(
      number = 2L,
      summaryFunction = twoClassSummary,
      classProbs = TRUE,
      savePredictions = TRUE
    )
  )
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2L:4L]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl = trainControl(number = 2L, savePredictions = TRUE))
  expect_output(summary(ens.class), "ROC")
  expect_output(summary(ens.reg), "RMSE")

  expect_is(plot(ens.class), "ggplot")
  expect_is(plot(ens.reg), "ggplot")
  expect_is(plot(ens.reg$models[[2L]]), "trellis")
  tp <- plot(ens.class)
  tp2 <- plot(ens.reg)
  expect_equal(nrow(tp$data), 4L)
  expect_equal(nrow(tp2$data), 3L)
  expect_equal(tp$data$model_name, names(ens.class$models))
  expect_equal(tp2$data$model_name, names(ens.reg$models))

  test_plot_file <- "caretEnsemble_test_plots.png"
  png(test_plot_file)
  suppressWarnings(autoplot(ens.class))
  suppressWarnings(autoplot(ens.reg))
  suppressWarnings(autoplot(ens.class, xvars = c("Petal.Length", "Petal.Width")))
  suppressWarnings(autoplot(ens.reg, xvars = c("Petal.Length", "Petal.Width")))
  expect_error(autoplot(ens.reg$models[[1L]]))
  dev.off()
  expect_true(file.exists(test_plot_file))
  unlink(test_plot_file)
})


context("Are ensembles construct accurately")

test_that("Do model results in caretEnsemble match component models - classification", {
  set.seed(2239L)
  ens.class <- caretEnsemble(
    models.class,
    metric = "ROC",
    trControl = trainControl(
      number = 2L, summaryFunction = twoClassSummary, classProbs = TRUE
    )
  )
  models.subset <- models.reg[2L:4L]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl = trainControl(number = 2L))
  modres1 <- extractModelMetrics(ens.class)
  modres2 <- extractModelMetrics(ens.reg)
  expect_is(modres2, "data.frame")
  expect_equal(modres2$model_name, names(models.subset))
})

test_that("Do model results in caretEnsemble match component models - regression", {
  ens.reg <- caretEnsemble(models.reg, trControl = trainControl(method = "none"))
  ens.class <- caretEnsemble(models.class, trControl = trainControl(method = "none"))
  newDat <- ens.class$models[[3L]]$trainingData
  newDat[2L, 2L] <- NA
  newDat[3L, 3L] <- NA
  newDat[4L, 4L] <- NA
  newDat <- newDat[1L:10L, ]

  # These yield errors on NAs because predict.randomForest can't handle NAs in new data
  expect_error(predict(ens.class, newdata = newDat, return_weights = TRUE, se = FALSE))
  expect_error(predict(ens.reg, newdata = newDat, return_weights = TRUE, se = TRUE))
  expect_error(predict(ens.reg, newdata = newDat, return_weights = FALSE, se = FALSE))
  expect_error(predict(ens.reg, newdata = newDat, return_weights = FALSE, se = TRUE))
})

# Reg tests
test_that("Prediction options are respected in regression and classification", {
  skip_on_cran()
  ens.reg <- caretEnsemble(models.reg, trControl = trainControl(method = "none"))
  tests <- expand.grid(se = 0L:1L, return_weights = 0L:1L)
  tests <- data.frame(lapply(tests, as.logical))
  for (i in seq_len(nrow(tests))) {
    suppressWarnings({
      p <- predict(
        ens.reg,
        se = tests[i, "se"],
        return_weights = tests[i, "return_weights"]
      )
    })

    if (tests[i, "se"]) {
      expect_is(p, "data.frame")
      preds <- p
    } else {
      expect_is(p, "numeric")
      preds <- p
    }

    if (tests[i, "return_weights"]) {
      expect_is(attr(preds, which = "weights")$Overall, "numeric")
    } else {
      expect_null(attr(preds, which = "weights"))
    }
  }

  # Class tests
  ens.class <- caretEnsemble(models.class, trControl = trainControl(method = "none"))
  tests <- expand.grid(se = 0L:1L, return_weights = 0L:1L)
  tests <- data.frame(lapply(tests, as.logical))
  for (i in seq_len(nrow(tests))) {
    suppressWarnings({
      p <- predict(
        ens.class,
        se = tests[i, "se"],
        return_weights = tests[i, "return_weights"],
        type = "prob"
      )
    })

    expect_is(p, "data.frame")
    preds <- p

    if (tests[i, "return_weights"]) {
      expect_is(unlist(attr(preds, which = "weights")), "numeric")
    } else {
      expect_null(attr(preds, which = "weights"))
    }
  }
})
