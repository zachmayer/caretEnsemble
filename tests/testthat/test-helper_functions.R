
#TODO: add tests for every helper function

context("Do the helper functions work for regression objects?")
library(caret)
library(randomForest)

test_that("We can make the predobs matrix", {
  data(models_reg)
  out <- makePredObsMatrix(models_reg)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 4)))
})

test_that("We can multiPredict", {
  data(models_reg)
  data(X.reg)
  out <- multiPredict(models_reg, 'reg', newdata=X.reg)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 4)))
  expect_true(all(colnames(out)==c("rf", "lm", "glm", "knn")))
})

context("Do the helper functions work for classification objects?")

test_that("We can make the predobs matrix", {
  data(models_class)
  out <- makePredObsMatrix(models_class)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 6)))

})

test_that("We can multiPredict", {
  data(models_class)
  data(X.class)
  data(X.reg)
  out <- multiPredict(models_class, 'reg', newdata=X.class)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 6)))
  expect_true(all(colnames(out)==c("rf", "glm", "svmRadial", "nnet", "treebag", "knn")))
})
