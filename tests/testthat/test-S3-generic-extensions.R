# test-S3-generic-extensions

set.seed(107)
library("caret")
library("caretEnsemble")
library("pROC")
library("randomForest")
library("rpart")
library("mlbench")

data(Sonar)


ctrl1 <- trainControl(method = "boot",
                      number = 5,
                      savePredictions = TRUE,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE,
                      verboseIter =TRUE,
                      index=createResample(Sonar$Class, 5))

# a model of class caretList
model_list1 <- caretList(Class ~ .,
                         data=Sonar,
                         trControl = ctrl1,
                         tuneList = list(
                           glm=caretModelSpec(method="glm", family="binomial"),
                           rpart=caretModelSpec(method="rpart")
                         ),
                         metric="ROC")

# a model of class train
rfTrain <- train(Class ~ .,
                 data=Sonar,
                 trControl = ctrl1,
                 method="rf")

###############################################
context("Ancillary caretList S3 Generic Functions Extensions")
################################################
test_that("c.caretEnsemble can bind two caretList objects", {

  model_list2 <- caretList(Class ~ .,
                           data=Sonar,
                           trControl = ctrl1,
                           tuneList = list(
                             glm=caretModelSpec(method="rpart"),
                             rpart=caretModelSpec(method="rf")
                           ),
                           metric="ROC")

  bigList <- c(model_list1, model_list2)
  ens1 <- caretEnsemble(bigList)

  expect_true(is.list(bigList))
  expect_true((  length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 4)
  expect_equal(class(ens1), "caretEnsemble")
})

test_that("c.caretEnsemble can bind a caretList and train object", {

  bigList <- c(model_list1, rfTrain)
  ens1 <- caretEnsemble(bigList)

  expect_true(is.list(bigList))
  expect_true((  length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 3)
  expect_equal(class(ens1), "caretEnsemble")
})

test_that("c.caretEnsemble can bind two objects of class train", {

  # a model of class train
  rpartTrain <- train(Class ~ .,
                      data=Sonar,
                      trControl = ctrl1,
                      method="rpart")

  bigList <- c(rfTrain, rpartTrain)
  ens1 <- caretEnsemble(bigList)

  expect_true(is.list(bigList))
  expect_true((  length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 2)
  expect_equal(class(ens1), "caretEnsemble")
})
