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

testthat::test_that("caret::varImp.caretEnsemble", {
  set.seed(2239L)

  for (m in list(ens.class, ens.reg)) {
    for (s in c(TRUE, FALSE)) {
      i <- caret::varImp(m, normalize = s)
      testthat::expect_is(i, "numeric")
      testthat::expect_length(i, length(m$models))
      testthat::expect_named(i, names(m$models))
      if (s) {
        testthat::expect_true(all(i >= 0.0))
        testthat::expect_true(all(i <= 1.0))
        testthat::expect_equal(sum(i), 1.0, tolerance = 1e-6)
      }
    }
  }
})

testthat::test_that("plot.caretEnsemble", {
  for (ens in list(ens.class, ens.reg)) {
    plt <- plot(ens)
    testthat::expect_is(plt, "ggplot")
    testthat::expect_identical(nrow(plt$data), 5L) # 4 models, one ensemble
    testthat::expect_named(ens$models, plt$data$model_name[-1L]) # First is ensemble
  }
})

testthat::test_that("ggplot2::autoplot.caretEnsemble", {
  for (ens in list(ens.class, ens.reg)) {
    plt1 <- ggplot2::autoplot(ens)
    plt2 <- ggplot2::autoplot(ens, xvars = c("Petal.Length", "Petal.Width"))

    testthat::expect_is(plt1, "ggplot")
    testthat::expect_is(plt2, "ggplot")

    testthat::expect_is(plt1, "patchwork")
    testthat::expect_is(plt2, "patchwork")

    train_model <- ens.reg$models[[1L]]
    testthat::expect_error(ggplot2::autoplot(train_model), "Objects of class (.*?) are not supported by autoplot")
  }
})

testthat::test_that("summary.caretEnsemble", {
  for (ens in list(ens.class, ens.reg)) {
    smry <- testthat::expect_silent(summary(ens.class))
    testthat::expect_output(print(smry), ens.class$ens_model$metric)
    for (name in names(ens.class$models)) {
      testthat::expect_output(print(smry), name)
    }
  }
})

testthat::test_that("extractModelMetrics", {
  for (ens in list(ens.class, ens.reg)) {
    metrics <- extractMetric(ens)
    testthat::expect_s3_class(metrics, "data.table")
    testthat::expect_named(ens$models, metrics$model_name[-1L])
  }
})

testthat::test_that("precict.caretEnsemble with and without se and weights", {
  for (ens in list(ens.class, ens.reg)) {
    is_class <- isClassifier(ens)
    for (se in c(FALSE, TRUE)) {
      p <- predict(
        ens,
        newdata = X.reg,
        se = se,
        excluded_class_id = 1L
      )
      expect_s3_class(p, "data.table")
      if (se) {
        testthat::expect_named(p, c("pred", "lwr", "upr"))
      } else {
        testthat::expect_named(p, ifelse(is_class, "Yes", "pred"))
      }
    }
  }
})
