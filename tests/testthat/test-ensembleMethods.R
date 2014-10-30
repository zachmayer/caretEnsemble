
context("Does ensembling and prediction work?")
library(caret)
library(randomForest)


test_that("We can ensemble regression models", {
  data(models_reg)
  data(X.reg)
  data(Y.reg)
  data(models_class)
  ens.class <- caretEnsemble(models_class, iter=1000)
  ens.reg <- caretEnsemble(models_reg, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})
