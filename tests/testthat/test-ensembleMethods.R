
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


test_that("We get warnings when scale is set to FALSE and weight is TRUE"){
  gives_warning(varImp(ens.reg, scale = FALSE, weight = TRUE))
  gives_warning(varImp(ens.class, scale = FALSE, weight = TRUE))
  expect_warning(varImp(ens.reg, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  gives_warning(varImp(ens.reg, scale = FALSE))
  gives_warning(varImp(ens.class, scale = FALSE))
  expect_warning(varImp(ens.reg, scale = FALSE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE),
                 "Weighting of unscaled")
}

ncol1 <- 7
ncol2 <- 4
nrow1 <- 6
nrow2 <- 6

test_that("We get the right dimensions back"){
  expect_equal(ncol(varImp(ens.class)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = FALSE)), ncol1-1)
  expect_equal(ncol(varImp(ens.class, weight = TRUE)), ncol1)
  expect_equal(ncol(varImp(ens.reg)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = FALSE)), ncol2-1)
  expect_equal(ncol(varImp(ens.reg, weight = TRUE)), ncol2)
  expect_equal(nrow(varImp(ens.class)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = FALSE)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = TRUE)), nrow1)
  expect_equal(nrow(varImp(ens.reg)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = FALSE)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = TRUE)), nrow2)
}
