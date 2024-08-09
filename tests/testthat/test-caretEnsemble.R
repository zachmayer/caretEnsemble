# Are tests failing here?
# UPDATE THE FIXTURES!
# make update-test-fixtures

utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)

utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)

utils::data(Sonar, package = "mlbench")

set.seed(1234L)
ens.reg <- caretEnsemble(
  models.reg,
  trControl = caret::trainControl(method = "cv", number = 2L, savePredictions = "final")
)

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
#############################################################################
testthat::context("Test metric and residual extraction")
#############################################################################

testthat::test_that("We can extract resdiuals from train regression objects", {
  mod <- caret::train(
    iris[, 1L:2L], iris[, 3L],
    method = "lm"
  )
  r <- stats::residuals(mod)
  testthat::expect_is(r, "numeric")
  testthat::expect_length(r, 150L)
})

#############################################################################
testthat::context("Does ensembling and prediction work?")
#############################################################################

testthat::test_that("We can ensemble regression models", {
  testthat::expect_s3_class(ens.reg, "caretEnsemble")
  pred.reg <- predict(ens.reg, newdata = X.reg)
  pred.reg2 <- predict(ens.reg, newdata = X.reg, se = TRUE)

  testthat::expect_true(all(pred.reg == pred.reg2$pred))


  testthat::expect_s3_class(pred.reg, "data.table")
  testthat::expect_identical(nrow(pred.reg), 150L)
  ens.class <- caretEnsemble(models.class)
  testthat::expect_s3_class(ens.class, "caretEnsemble")
  pred.class <- predict(ens.class, newdata = X.class)
  testthat::expect_s3_class(pred.class, "data.table")
  testthat::expect_identical(nrow(pred.class), 150L)
})

#############################################################################
testthat::context("Does ensembling work with models with differing predictors")
#############################################################################

testthat::test_that("We can ensemble models of different predictors", {
  Y.reg <- iris[, 1L]
  X.reg <- model.matrix(~., iris[, -1L])
  mseeds <- vector(mode = "list", length = 12L)
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

  # Can we predict from the list
  pred_list <- predict(nestedList, newdata = X.reg)
  testthat::expect_s3_class(pred_list, "data.table")
  testthat::expect_identical(nrow(pred_list), 150L)
  testthat::expect_identical(ncol(pred_list), length(nestedList))

  # Can we predict from the ensemble
  ensNest <- caretEnsemble(nestedList)
  testthat::expect_s3_class(ensNest, "caretEnsemble")
  pred.nest <- predict(ensNest, newdata = X.reg)
  testthat::expect_s3_class(pred.nest, "data.table")
  testthat::expect_identical(nrow(pred.nest), 150L)

  # Ensemble errors on NAs
  X_reg_new <- X.reg
  X_reg_new[2L, 3L] <- NA
  expect_error(
    predict(ensNest, newdata = X_reg_new),
    "is.finite(newdata) are not all TRUE",
    fixed = TRUE
  )
})

testthat::context("Does ensemble prediction work with new data")

testthat::test_that("caretEnsemble works for regression models", {
  set.seed(1234L)
  testthat::expect_is(ens.reg, "caretEnsemble")

  # Predictions
  pred_stacked <- predict(ens.reg) # stacked predictions
  pred_in_sample <- predict(ens.reg, newdata = X.reg) # in sample predictions
  pred_one <- predict(ens.reg, newdata = X.reg[2L, , drop = FALSE]) # one row predictions

  # Check class
  testthat::expect_s3_class(pred_stacked, "data.table")
  testthat::expect_s3_class(pred_in_sample, "data.table")
  testthat::expect_s3_class(pred_one, "data.table")

  # Check len
  testthat::expect_identical(nrow(pred_stacked), 150L)
  testthat::expect_identical(nrow(pred_in_sample), 150L)
  testthat::expect_identical(nrow(pred_one), 1L)

  # stacked predcitons should be similar to in sample predictions
  testthat::expect_equal(pred_stacked, pred_in_sample, tol = 0.1)

  # One row predictions
  testthat::expect_equivalent(pred_one$pred, 4.712639, tol = 0.05)
})

testthat::test_that("caretEnsemble works for classification models", {
  set.seed(1234L)
  ens.class <- caretEnsemble(
    models.class,
    trControl = caret::trainControl(
      method = "cv",
      number = 10L,
      savePredictions = "final",
      classProbs = TRUE
    )
  )
  testthat::expect_s3_class(ens.class, "caretEnsemble")
  ens.class$ens_model$finalModel

  # Predictions
  pred_stacked <- predict(ens.class) # stacked predictions
  pred_in_sample <- predict(ens.class, newdata = X.class) # in sample predictions
  pred_one <- predict(ens.class, newdata = X.class[2L, , drop = FALSE]) # one row predictions

  # Check class
  testthat::expect_s3_class(pred_stacked, "data.table")
  testthat::expect_s3_class(pred_in_sample, "data.table")
  testthat::expect_s3_class(pred_one, "data.table")

  # Check rows
  testthat::expect_identical(nrow(pred_stacked), 150L)
  testthat::expect_identical(nrow(pred_in_sample), 150L)
  testthat::expect_identical(nrow(pred_one), 1L)

  # Check cols
  testthat::expect_identical(ncol(pred_stacked), 2L)
  testthat::expect_identical(ncol(pred_in_sample), 2L)
  testthat::expect_identical(ncol(pred_one), 2L)

  # stacked predcitons should be similar to in sample predictions
  testthat::expect_equal(pred_stacked, pred_in_sample, tol = 0.1)

  # One row predictions
  testthat::expect_equivalent(pred_one$Yes, 0.02, tol = 0.05)
  testthat::expect_equivalent(pred_one$No, 0.98, tol = 0.05)
})

testthat::context("Do ensembles of custom models work?")

testthat::test_that("Ensembles using custom models work correctly", {
  set.seed(1234L)

  # Create custom caret models with a properly assigned method attribute
  custom.rf <- getModelInfo("rf", regex = FALSE)[[1L]]
  custom.rf$method <- "custom.rf"

  custom.rpart <- getModelInfo("rpart", regex = FALSE)[[1L]]
  custom.rpart$method <- "custom.rpart"

  # Define models to be used in ensemble
  # Add an unnamed model to ensure that method names are extracted from model info
  # Add a named custom model, to contrast the above
  # Add a non-custom model
  tune.list <- list(
    caretModelSpec(method = custom.rf, tuneLength = 1L),
    myrpart = caretModelSpec(method = custom.rpart, tuneLength = 1L),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )

  # Create an ensemble using the above models
  cl <- caretList(X.class, Y.class, tuneList = tune.list)
  cs <- caretEnsemble(
    cl,
    trControl = caret::trainControl(
      method = "cv",
      number = 2L,
      savePredictions = "final",
      classProbs = TRUE
    )
  )
  testthat::expect_is(cs, "caretEnsemble")

  # Validate names assigned to ensembled models
  testthat::expect_named(cs$models, c("custom.rf", "myrpart", "treebag"))

  # Validate ensemble predictions
  pred_stacked <- predict(cs) # stacked predictions
  pred_in_sample <- predict(cs, newdata = X.class) # in sample predictions
  pred_one <- predict(cs, newdata = X.class[2L, , drop = FALSE]) # one row predictions

  # Check class
  testthat::expect_s3_class(pred_stacked, "data.table")
  testthat::expect_s3_class(pred_in_sample, "data.table")
  testthat::expect_s3_class(pred_one, "data.table")

  # Check rows
  testthat::expect_identical(nrow(pred_stacked), 150L)
  testthat::expect_identical(nrow(pred_in_sample), 150L)
  testthat::expect_identical(nrow(pred_one), 1L)

  # Check cols
  testthat::expect_identical(ncol(pred_stacked), 2L)
  testthat::expect_identical(ncol(pred_in_sample), 2L)
  testthat::expect_identical(ncol(pred_one), 2L)

  # stacked predcitons should be similar to in sample predictions
  # These differ a lot!
  testthat::expect_equal(pred_stacked, pred_in_sample, tol = 0.4)

  # One row predictions
  testthat::expect_equivalent(pred_one$Yes, 0.07557944, tol = 0.1)
  testthat::expect_equivalent(pred_one$No, 0.9244206, tol = 0.1)

  # Verify that not specifying a method attribute for custom models causes an error
  #  Add a custom caret model WITHOUT a properly assigned method attribute
  tune.list <- list(
    caretModelSpec(method = getModelInfo("rf", regex = FALSE)[[1L]], tuneLength = 1L),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )
  msg <- "Custom models must be defined with a \"method\" attribute"
  testthat::expect_error(caretList(X.class, Y.class, tuneList = tune.list, trControl = train.control), regexp = msg)
})

testthat::test_that("Ensembles fails if predictions are not saved", {
  models_bad <- models.reg[[1L]]
  models_bad$pred <- NULL
  testthat::expect_error(
    stackedTrainResiduals(models_bad),
    "No predictions saved during training. Please set savePredictions = 'final' in trainControl"
  )
})

testthat::test_that("caret::varImp.caretEnsemble", {
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

testthat::test_that("We can train and ensemble models with custom tuning lists", {
  target <- "Class"

  custom_list <- caretList(
    x = Sonar[, setdiff(names(Sonar), target)],
    y = Sonar[, target],
    tuneList = list(
      rpart = caretModelSpec(
        method = "rpart",
        tuneGrid = data.table::data.table(.cp = c(0.01, 0.001, 0.1, 1.0)) # 4L parameters
      ),
      knn = caretModelSpec(
        method = "knn",
        tuneLength = 9L # 9L parameters
      ),
      lda = caretModelSpec(
        method = "lda2",
        tuneLength = 1L # 1L parameters
      ),
      nnet = caretModelSpec(
        method = "nnet",
        tuneLength = 2L, # 2 paramters.  2 values each.   2^2 = 4
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

  # Ensemble them
  custom_ensemble <- caretEnsemble(custom_list)
  testthat::expect_is(custom_ensemble, "caretEnsemble")
})
