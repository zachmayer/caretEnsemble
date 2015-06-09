
context("Does stacking and prediction work?")
library(caret)
library(randomForest)

load(system.file("testdata/models.reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/X.reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/Y.reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/models.class.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/X.class.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/Y.class.rda",
                 package="caretEnsemble", mustWork=TRUE))

test_that("We can stack regression models", {
  set.seed(96367)
  ens.reg <- caretStack(models.reg, method="lm", preProcess="pca",
                        trControl=trainControl(number=2, allowParallel=FALSE))
  expect_that(ens.reg, is_a("caretStack"))
  pred.reg <- predict(ens.reg, X.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})

test_that("We can stack classification models", {
  set.seed(42)
  ens.class <- caretStack(models.class, method="rpart",
                          trControl=trainControl(number=2, allowParallel=FALSE))
  expect_that(ens.class, is_a("caretStack"))
  pred.class <- predict(ens.class, X.class, type="prob")[,2]
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)
  raw.class <- predict(ens.class, X.class, type="raw")
  expect_true(is.factor(raw.class))
  expect_true(length(raw.class)==150)
})
