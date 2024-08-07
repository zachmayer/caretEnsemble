# Are tests failing here?
# UPDATE THE FIXTURES!
# make update-test-fixtures

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

set.seed(1234L)
ens.reg <- caretEnsemble(
  models.reg,
  trControl = caret::`trainControl`(method = "cv", number = 2L, savePredictions = "final")
)

#############################################################################
testthat::context("Test metric and residual extraction")
#############################################################################

testthat::test_that("We can extract resdiuals from train regression objects", {
  data(iris)
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
  data(iris)
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
