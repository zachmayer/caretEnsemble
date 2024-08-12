# Load required data
utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)
utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)
utils::data(Sonar, package = "mlbench")

# Set up test environment
set.seed(1234L)
k <- 2L
ens.reg <- caretEnsemble(
  models.reg,
  trControl = caret::trainControl(
    method = "cv",
    number = k,
    index = caret::createFolds(Y.reg, k = k),
    savePredictions = "final"
  )
)

ens.class <- caretEnsemble(
  models.class,
  metric = "ROC",
  trControl = caret::trainControl(
    method = "cv",
    number = k,
    index = caret::createFolds(Y.class, k = k),
    summaryFunction = caret::twoClassSummary,
    classProbs = TRUE,
    savePredictions = TRUE
  )
)

# Helper function for prediction tests
test_predictions <- function(ens, newdata, one_row_preds) {
  is_class <- isClassifier(ens)
  N <- nrow(newdata)

  pred_stacked <- predict(ens)
  pred <- predict(ens, newdata = newdata)
  pred_se <- predict(ens.reg, newdata = X.reg, se = TRUE)
  pred_one <- predict(ens, newdata = newdata[1L, , drop = FALSE])

  testthat::expect_s3_class(pred_stacked, "data.table")
  testthat::expect_s3_class(pred, "data.table")
  testthat::expect_s3_class(pred_se, "data.table")
  testthat::expect_s3_class(pred_one, "data.table")

  testthat::expect_identical(nrow(pred_stacked), N)
  testthat::expect_identical(nrow(pred), N)
  testthat::expect_identical(nrow(pred_se), N)
  testthat::expect_identical(nrow(pred_one), 1L)

  testthat::expect_equal(pred_stacked, pred, tol = ifelse(is_class, 0.35, 0.05))

  if (is_class) {
    testthat::expect_identical(ncol(pred_stacked), 2L)
    testthat::expect_identical(ncol(pred), 2L)
    testthat::expect_identical(ncol(pred_one), 2L)
    testthat::expect_equivalent(pred_one$Yes, one_row_preds[1L], tol = 0.05)
    testthat::expect_equivalent(pred_one$No, one_row_preds[2L], tol = 0.05)
  } else {
    testthat::expect_equivalent(pred_one$pred, one_row_preds[1L], tol = 0.05)
  }
}

######################################################################
testthat::context("Metric and residual extraction")
######################################################################

testthat::test_that("We can extract residuals from train regression objects", {
  data(iris)
  mod <- caret::train(iris[, 1L:2L], iris[, 3L], method = "lm")
  r <- stats::residuals(mod)
  testthat::expect_is(r, "numeric")
  testthat::expect_length(r, 150L)
})

######################################################################
testthat::context("Ensembling and prediction")
######################################################################

testthat::test_that("We can ensemble regression models", {
  testthat::expect_s3_class(ens.reg, "caretEnsemble")
  test_predictions(ens.reg, X.reg, 5.04)
})

testthat::test_that("We can ensemble classification models", {
  testthat::expect_s3_class(ens.class, "caretEnsemble")
  test_predictions(ens.class, X.class, c(0.02, 0.98))
})

######################################################################
testthat::context("Ensembling with models of differing predictors")
######################################################################

testthat::test_that("We can ensemble models of different predictors", {
  data(iris)
  Y.reg <- iris[, 1L]
  X.reg <- model.matrix(~., iris[, -1L])
  my_control <- caret::trainControl(
    method = "cv", number = 2L,
    p = 0.75,
    savePrediction = TRUE,
    returnResamp = "final"
  )

  set.seed(482L)
  nestedList <- list(
    glm1 = caret::train(x = X.reg[, c(-1L, -2L, -6L)], y = Y.reg, method = "glm", trControl = my_control),
    glm2 = caret::train(x = X.reg[, c(-1L, -3L, -6L)], y = Y.reg, method = "glm", trControl = my_control),
    glm3 = caret::train(x = X.reg[, c(-1L, -2L, -3L, -6L)], y = Y.reg, method = "glm", trControl = my_control),
    glm4 = caret::train(x = X.reg[, c(-1L, -4L, -6L)], y = Y.reg, method = "glm", trControl = my_control)
  )
  nestedList <- as.caretList(nestedList)

  pred_list <- predict(nestedList, newdata = X.reg)
  testthat::expect_s3_class(pred_list, "data.table")
  testthat::expect_identical(nrow(pred_list), 150L)
  testthat::expect_identical(ncol(pred_list), length(nestedList))

  ensNest <- caretEnsemble(nestedList)
  testthat::expect_s3_class(ensNest, "caretEnsemble")
  pred.nest <- predict(ensNest, newdata = X.reg)
  testthat::expect_s3_class(pred.nest, "data.table")
  testthat::expect_identical(nrow(pred.nest), 150L)

  X_reg_new <- X.reg
  X_reg_new[2L, 3L] <- NA
  testthat::expect_error(
    predict(ensNest, newdata = X_reg_new),
    "is.finite(newdata) are not all TRUE",
    fixed = TRUE
  )
})

######################################################################
testthat::context("Ensembles with custom models")
######################################################################

testthat::test_that("Ensembles using custom models work correctly", {
  set.seed(1234L)

  custom.rf <- getModelInfo("rf", regex = FALSE)[[1L]]
  custom.rf$method <- "custom.rf"

  custom.rpart <- getModelInfo("rpart", regex = FALSE)[[1L]]
  custom.rpart$method <- "custom.rpart"

  tune.list <- list(
    caretModelSpec(method = custom.rf, tuneLength = 1L),
    myrpart = caretModelSpec(method = custom.rpart, tuneLength = 1L),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )

  cl <- caretList(X.class, Y.class, tuneList = tune.list)
  cs <- caretEnsemble(cl)
  testthat::expect_is(cs, "caretEnsemble")
  testthat::expect_named(cs$models, c("custom.rf", "myrpart", "treebag"))

  test_predictions(cs, X.class, c(0.0198, 0.9802))

  tune.list_bad <- list(
    caretModelSpec(method = getModelInfo("rf", regex = FALSE)[[1L]], tuneLength = 1L),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )
  testthat::expect_error(
    caretList(X.class, Y.class, tuneList = tune.list_bad, trControl = train.control),
    "Custom models must be defined with a \"method\" attribute"
  )
})

testthat::test_that("Ensembles fails if predictions are not saved", {
  models_bad <- models.reg[[1L]]
  models_bad$pred <- NULL
  testthat::expect_error(
    stackedTrainResiduals(models_bad),
    "No predictions saved during training. Please set savePredictions = 'final' in trainControl"
  )
})

######################################################################
testthat::context("Variable importance and plotting")
######################################################################

testthat::test_that("caret::varImp.caretEnsemble works", {
  set.seed(2239L)

  for (m in list(ens.class, ens.reg)) {
    for (s in c(TRUE, FALSE)) {
      i <- caret::varImp(m, normalize = s)
      testthat::expect_is(i, "numeric")
      if (isClassifier(m)) {
        len <- length(m$models) * 2L
        n <- c(outer(c("rf", "glm", "rpart", "treebag"), c("No", "Yes"), paste, sep = "_"))
        n <- matrix(n, ncol = 2L)
        n <- c(t(n))
      } else {
        len <- length(m$models)
        n <- names(m$models)
      }
      testthat::expect_length(i, len)
      testthat::expect_named(i, n)
      if (s) {
        testthat::expect_true(all(i >= 0.0))
        testthat::expect_true(all(i <= 1.0))
        testthat::expect_equal(sum(i), 1.0, tolerance = 1e-6)
      }
    }
  }
})

testthat::test_that("plot.caretEnsemble works", {
  for (ens in list(ens.class, ens.reg)) {
    plt <- plot(ens)
    testthat::expect_is(plt, "ggplot")
    testthat::expect_identical(nrow(plt$data), 5L)
    testthat::expect_named(ens$models, plt$data$model_name[-1L])
  }
})

testthat::test_that("ggplot2::autoplot.caretEnsemble works", {
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

testthat::test_that("summary.caretEnsemble works", {
  for (ens in list(ens.class, ens.reg)) {
    smry <- testthat::expect_silent(summary(ens.class))
    testthat::expect_output(print(smry), ens.class$ens_model$metric)
    for (name in names(ens.class$models)) {
      testthat::expect_output(print(smry), name)
    }
  }
})

testthat::test_that("predict.caretEnsemble works with and without se and weights", {
  for (ens in list(ens.class, ens.reg)) {
    is_class <- isClassifier(ens)
    for (se in c(FALSE, TRUE)) {
      p <- predict(
        ens,
        newdata = X.reg,
        se = se,
        excluded_class_id = 1L
      )
      testthat::expect_s3_class(p, "data.table")
      if (se) {
        testthat::expect_named(p, c("pred", "lwr", "upr"))
      } else {
        testthat::expect_named(p, ifelse(is_class, "Yes", "pred"))
      }
    }
  }
})

testthat::test_that("We can train and ensemble models with custom tuning lists", {
  target <- "Class"

  custom_list <- caretList(
    x = Sonar[, setdiff(names(Sonar), target)],
    y = Sonar[, target],
    tuneList = list(
      rpart = caretModelSpec(
        method = "rpart",
        tuneGrid = data.table::data.table(.cp = c(0.01, 0.001, 0.1, 1.0))
      ),
      knn = caretModelSpec(
        method = "knn",
        tuneLength = 9L
      ),
      lda = caretModelSpec(
        method = "lda2",
        tuneLength = 1L
      ),
      nnet = caretModelSpec(
        method = "nnet",
        tuneLength = 2L,
        trace = FALSE,
        softmax = FALSE
      )
    )
  )
  testthat::expect_is(custom_list, "caretList")
  testthat::expect_identical(nrow(custom_list[["rpart"]]$results), 4L)
  testthat::expect_identical(nrow(custom_list[["knn"]]$results), 9L)
  testthat::expect_identical(nrow(custom_list[["lda"]]$results), 1L)
  testthat::expect_identical(nrow(custom_list[["nnet"]]$results), 4L)
  testthat::expect_false(custom_list[["nnet"]]$finalModel$softmax)

  custom_ensemble <- caretEnsemble(custom_list)
  testthat::expect_is(custom_ensemble, "caretEnsemble")
})
