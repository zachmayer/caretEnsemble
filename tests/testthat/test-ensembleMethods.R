
context("Does variable importance work?")
library(caret)
library(randomForest)

data(models_reg)
data(X.reg)
data(Y.reg)
data(models_class)
ens.class <- caretEnsemble(models_class, iter=1000)
# varImp struggles with the rf in our test suite, why?
ens.reg <- caretEnsemble(models_reg[2:4], iter=1000)

test_that("We can get variable importance in classification models", {
  expect_is(varImp(ens.class), "data.frame")
  expect_is(varImp(ens.class, scale = FALSE), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("We can get variable importance in regression models", {
  expect_is(varImp(ens.reg), "data.frame")
  expect_is(varImp(ens.reg, scale = FALSE), "data.frame")
  expect_is(varImp(ens.reg, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg, scale = TRUE, weight = TRUE), "data.frame")
})
