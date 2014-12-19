# Test caretList
library('caret')
library('randomForest')
library('rpart')
library('gbm')
library('kernlab')
library('nnet')
library('ipred')

###############################################
context("Ancillary caretList functions and errors")
################################################
test_that("caretModelSpec returns valid specs", {
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)
  tuneList <- list(
    rf1=caretModelSpec(),
    rf2=caretModelSpec(method='rf', tuneLength=5),
    caretModelSpec(method='rpart'),
    caretModelSpec(method='knn', tuneLength=10)
    )
  tuneList <- tuneCheck(tuneList)
  expect_true(is.list(tuneList))
  expect_equal(length(tuneList), 4)
  expect_equal(sum(duplicated(names(tuneList))), 0)
})
