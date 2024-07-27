# Are tests failing here?
# UPDATE THE FIXTURES!
# make update-test-fixtures

library(testthat)
library(caret)

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

#############################################################################
context("Test errors and warnings")
#############################################################################
test_that("Ensembling fails with no CV", {
  my_control <- trainControl(method = "none", savePredictions = "final")
  expect_error(expect_warning(trControlCheck(my_control)))
})

#############################################################################
context("Test metric and residual extraction")
#############################################################################

test_that("We can extract metrics", {
  data(iris)
  mod <- train(
    iris[, 1L:2L], iris[, 3L],
    method = "lm",
    trControl = trainControl(
      method = "cv", number = 3L, savePredictions = "final"
    )
  )
  m1 <- getMetric(mod, "RMSE")
  m2 <- getMetric(mod, "RMSE")
  expect_equal(m1, m2)

  m1 <- getMetric(mod, "RMSE", return_sd = TRUE)
  m2 <- getMetric(mod, "RMSE", return_sd = TRUE)
  expect_equal(m1, m2)
})

test_that("We can extract resdiuals from train regression objects", {
  data(iris)
  mod <- train(
    iris[, 1L:2L], iris[, 3L],
    method = "lm",
    trControl = trainControl(
      method = "cv", number = 3L, savePredictions = "final"
    )
  )
  r <- residuals(mod)
  expect_is(r, "numeric")
  expect_length(r, 150L)
})

#############################################################################
context("Does ensembling and prediction work?")
#############################################################################

test_that("We can ensemble regression models", {
  ens.reg <- caretEnsemble(models.reg, trControl = trainControl(number = 2L))
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg)
  pred.reg2 <- predict(ens.reg, se = TRUE)

  expect_true(all(pred.reg == pred.reg2$fit))

  expect_error(predict(ens.reg, return_weights = "BOGUS"))

  expect_type(pred.reg, "double")
  expect_length(pred.reg, 150L)
  ens.class <- caretEnsemble(models.class, trControl = trainControl(number = 2L))
  expect_that(ens.class, is_a("caretEnsemble"))
  pred.class <- predict(ens.class, type = "prob")
  expect_s3_class(pred.class, "data.frame")
  expect_equal(nrow(pred.class), 150L)

  # Check different cases
  p1 <- predict(ens.reg, return_weights = TRUE, se = FALSE)
  expect_is(unlist(attr(p1, which = "weights")), "numeric")
  expect_is(p1, "numeric")

  p2 <- predict(ens.reg, return_weights = TRUE, se = TRUE)
  expect_is(unlist(attr(p2, which = "weights")), "numeric")
  expect_is(p2, "data.frame")
  expect_equal(ncol(p2), 3L)
  expect_named(p2, c("fit", "lwr", "upr"))

  p3 <- predict(ens.reg, return_weights = FALSE, se = FALSE)
  expect_is(p3, "numeric")
  expect_equivalent(p1, p3)
  expect_false(identical(p1, p3))

  expect_equivalent(p2$fit, p1)
  expect_equivalent(p2$fit, p3)
  expect_null(attr(p3, which = "weights"))
})

#############################################################################
context("Does ensembling work with models with differing predictors")
#############################################################################

test_that("We can ensemble models of different predictors", {
  data(iris)
  Y.reg <- iris[, 1L]
  X.reg <- model.matrix(~., iris[, -1L])
  mseeds <- vector(mode = "list", length = 12L)
  myControl <- trainControl(
    method = "cv", number = 10L,
    p = 0.75, savePrediction = TRUE,
    classProbs = FALSE, returnResamp = "final",
    returnData = TRUE
  )

  set.seed(482L)
  glm1 <- train(x = X.reg[, c(-1L, -2L, -6L)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482L)
  glm2 <- train(x = X.reg[, c(-1L, -3L, -6L)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482L)
  glm3 <- train(x = X.reg[, c(-1L, -2L, -3L, -6L)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482L)
  glm4 <- train(x = X.reg[, c(-1L, -4L, -6L)], y = Y.reg, method = "glm", trControl = myControl)

  nestedList <- list(glm1 = glm1, glm2 = glm2, glm3 = glm3, glm4 = glm4)
  nestedList <- as.caretList(nestedList)

  # Can we predict from the list
  pred_list <- predict(nestedList, newdata = X.reg)
  expect_s3_class(pred_list, "data.table")
  expect_equal(nrow(pred_list), 150L)
  expect_equal(ncol(pred_list), length(nestedList))

  # Can we predict from the ensemble
  ensNest <- caretEnsemble(nestedList, trControl = trainControl(number = 2L))
  expect_s3_class(ensNest, "caretEnsemble")
  pred.nest <- predict(ensNest, newdata = X.reg)
  expect_type(pred.nest, "double")
  expect_length(pred.nest, 150L)

  X_reg_new <- X.reg
  X_reg_new[2L, 3L] <- NA
  X_reg_new[25L, 3L] <- NA
  p_with_nas <- predict(ensNest, newdata = X_reg_new)
})

context("Does ensemble prediction work with new data")

test_that("caretEnsemble works for regression models", {
  set.seed(1234L)
  ens.reg <- caretEnsemble(models.reg, trControl = trainControl(number = 2L))
  expect_is(ens.reg, "caretEnsemble")

  # Predictions
  pred_stacked <- predict(ens.reg) # stacked predictions
  pred_in_sample <- predict(ens.reg, newdata = X.reg) # in sample predictions
  pred_one <- predict(ens.reg, newdata = X.reg[2L, , drop = FALSE]) # one row predictions

  # Check class
  expect_is(pred_stacked, "numeric")
  expect_is(pred_in_sample, "numeric")
  expect_is(pred_one, "numeric")

  # Check len
  expect_length(pred_stacked, 150L)
  expect_length(pred_in_sample, 150L)
  expect_length(pred_one, 1L)

  # stacked predcitons should be similar to in sample predictions
  expect_equal(pred_stacked, pred_in_sample, tol = 0.1)

  # One row predictions
  expect_equivalent(pred_one, 4.712639, tol = 0.05)
})

test_that("caretEnsemble works for classification models", {
  set.seed(1234L)
  ens.class <- caretEnsemble(models.class, trControl = trainControl(number = 2L))
  expect_that(ens.class, is_a("caretEnsemble"))

  # Predictions
  pred_stacked <- predict(ens.class, type = "prob") # stacked predictions
  pred_in_sample <- predict(ens.class, newdata = X.class, type = "prob") # in sample predictions
  pred_one <- predict(ens.class, newdata = X.class[2L, , drop = FALSE], type = "prob") # one row predictions

  # Check class
  expect_is(pred_stacked, "data.frame")
  expect_is(pred_in_sample, "data.frame")
  expect_is(pred_one, "data.frame")

  # Check rows
  expect_equal(nrow(pred_stacked), 150L)
  expect_equal(nrow(pred_in_sample), 150L)
  expect_equal(nrow(pred_one), 1L)

  # Check cols
  expect_equal(ncol(pred_stacked), 2L)
  expect_equal(ncol(pred_in_sample), 2L)
  expect_equal(ncol(pred_one), 2L)

  # stacked predcitons should be similar to in sample predictions
  expect_equal(pred_stacked, pred_in_sample, tol = 0.1)

  # One row predictions
  expect_equivalent(pred_one$Yes, 0.03833661, tol = 0.05)
  expect_equivalent(pred_one$No, 0.9616634, tol = 0.05)
})

context("Do ensembles of custom models work?")

test_that("Ensembles using custom models work correctly", {
  set.seed(1234L)

  # Create custom caret models with a properly assigned method attribute
  custom.rf <- getModelInfo("rf", regex = FALSE)[[1L]]
  custom.rf$method <- "custom.rf"

  custom.rpart <- getModelInfo("rpart", regex = FALSE)[[1L]]
  custom.rpart$method <- "custom.rpart"

  # Define models to be used in ensemble
  tune.list <- list(
    # Add an unnamed model to ensure that method names are extracted from model info
    caretModelSpec(method = custom.rf, tuneLength = 1L),
    # Add a named custom model, to contrast the above
    myrpart = caretModelSpec(method = custom.rpart, tuneLength = 1L),
    # Add a non-custom model
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )
  train.control <- trainControl(method = "cv", number = 2L, classProbs = TRUE)

  # Create an ensemble using the above models
  expect_warning(cl <- caretList(X.class, Y.class, tuneList = tune.list, trControl = train.control))
  expect_is(cl, "caretList")
  expect_silent(cs <- caretEnsemble(cl))
  expect_is(cs, "caretEnsemble")

  # Validate names assigned to ensembled models
  expect_equal(sort(names(cs$models)), c("custom.rf", "myrpart", "treebag"))

  # Validate ensemble predictions
  pred_stacked <- predict(cs, type = "prob") # stacked predictions
  pred_in_sample <- predict(cs, newdata = X.class, type = "prob") # in sample predictions
  pred_one <- predict(cs, newdata = X.class[2L, , drop = FALSE], type = "prob") # one row predictions

  # Check class
  expect_is(pred_stacked, "data.frame")
  expect_is(pred_in_sample, "data.frame")
  expect_is(pred_one, "data.frame")

  # Check rows
  expect_equal(nrow(pred_stacked), 150L)
  expect_equal(nrow(pred_in_sample), 150L)
  expect_equal(nrow(pred_one), 1L)

  # Check cols
  expect_equal(ncol(pred_stacked), 2L)
  expect_equal(ncol(pred_in_sample), 2L)
  expect_equal(ncol(pred_one), 2L)

  # stacked predcitons should be similar to in sample predictions
  # These differ a lot!
  expect_equal(pred_stacked, pred_in_sample, tol = 0.4)

  # One row predictions
  expect_equivalent(pred_one$Yes, 0.05072556, tol = 0.05)
  expect_equivalent(pred_one$No, 0.9492744, tol = 0.05)

  # Verify that not specifying a method attribute for custom models causes an error
  tune.list <- list(
    # Add a custom caret model WITHOUT a properly assigned method attribute
    caretModelSpec(method = getModelInfo("rf", regex = FALSE)[[1L]], tuneLength = 1L),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1L)
  )
  msg <- "Custom models must be defined with a \"method\" attribute"
  expect_error(caretList(X.class, Y.class, tuneList = tune.list, trControl = train.control), regexp = msg)
})
