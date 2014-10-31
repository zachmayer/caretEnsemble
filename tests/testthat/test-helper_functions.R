
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

context("Test weighted standard deviations")

x <- rnorm(1000)
y <- runif(1000)


x1 <- c(3, 5, 9, 3, 4, 6, 4)
x2 <- c(10, 10, 20, 14, 2, 2, 40)
y <- c(10, 10, 10, 20)
w1 <- c(0.1, 0.1, 0.1, 0.7)

test_that("wtd.sd applies weights correctly", {
  expect_equal(sd(x), wtd.sd(x))
  expect_false(sd(x1) == wtd.sd(x1, weights = x2))
  expect_false(sd(x1) == wtd.sd(x1, weights = x2, normwt=TRUE))
  expect_true(sd(x1) == wtd.sd(x1))
  expect_equal(sd(y), 5)
  expect_equal(wtd.sd(y, weights = w1), 4.582576, tolerance = .001)
  expect_equal(wtd.sd(y, weights = w1), wtd.sd(y, weights = w1, normwt=TRUE))
  expect_equal(wtd.sd(y, weights = w1*100), wtd.sd(y, weights = w1*100, normwt=TRUE))
})
