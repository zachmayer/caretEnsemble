
########################################################################
context("Do the helper functions work for regression objects?")
########################################################################
library("caret")
library("randomForest")
library("rpart")

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

test_that("Recycling generates a warning", {
  expect_error(caretEnsemble:::wtd.sd(matrix(1:10, ncol=2), w=1))
})

test_that("No predictions generates an error", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("gbm")
  skip_if_not_installed("plyr")
  skip_if_not_installed("glmnet")
  expect_warning(
    models_multi <- caretList(
      iris[, 1:2], iris[, 5],
      tuneLength=1, verbose=FALSE,
      methodList=c("rf", "gbm"),
      trControl=trainControl(method="cv", number=2, savePredictions="final", classProbs=TRUE))
  )
  expect_error(check_caretList_model_types(models_multi))

  expect_warning(
    models <- caretList(
      iris[, 1:2], factor(ifelse(iris[, 5]=="setosa", "Yes", "No")),
      tuneLength=1, verbose=FALSE,
      methodList=c("rf", "gbm"),
      trControl=trainControl(method="cv", number=2, savePredictions="final", classProbs=TRUE))
  )
  new_model <- train(
    iris[, 1:2], factor(ifelse(iris[, 5]=="setosa", "Yes", "No")),
    tuneLength=1,
    method=c("glmnet"),
    trControl=trainControl(method="cv", number=2, savePredictions="none", classProbs=TRUE)
  )
  models2 <- c(new_model, models)
  models3 <- c(models, new_model)
  check_caretList_model_types(models)
  expect_error(check_caretList_model_types(models2))
  #expect_error(check_caretList_model_types(models3)) #THIS IS A BUG THAT NEEDS FIXING!!!!!!!!!!
})

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.reg)
  expect_is(out, "list")
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 4)))
})

test_that("We can predict", {
  expect_warning(out <- predict(models.reg, "reg", newdata=X.reg))
  expect_is(out, "matrix")
  expect_true(all(dim(out)==c(150, 4)))
  expect_true(all(colnames(out)==c("rf", "glm", "rpart", "treebag")))
})

########################################################################
context("Do the helper functions work for classification objects?")
########################################################################

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.class)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 4)))
})

test_that("We can predict", {
  expect_warning(out <- predict(models.class, "Classification", newdata=X.class))
  expect_is(out, "matrix")
  expect_true(all(dim(out)==c(150, 4)))
  expect_true(all(colnames(out)==c("rf", "glm", "rpart", "treebag")))
  expect_warning(out2 <- predict(models.reg, "Regression", newdata = X.reg))
  expect_true(all(dim(out2)==c(150, 4)))
  expect_true(all(colnames(out2)==c("rf", "glm", "rpart", "treebag")))
})

test_that("predict results same regardless of verbose option", {
  sink <- capture.output({
    expect_warning({
      expect_is(predict(models.class, "Classification", newdata = X.class), "matrix")
      out1 <- predict(models.class, "Classification", newdata = X.class)
      out2 <- predict(models.class, "Classification", verbose = TRUE, newdata = X.class)
      expect_identical(out1, out2)

    })
    expect_warning({
      expect_is(predict(models.reg, "Regression", newdata = X.reg), "matrix")
      out1 <- predict(models.reg, "Regression", newdata = X.reg)
      out2 <- predict(models.reg, "Regression", verbose = TRUE, newdata = X.reg)
      expect_identical(out1, out2)
    })
  })
})

context("Test weighted standard deviations")

x <- rnorm(1000)
x1 <- c(3, 5, 9, 3, 4, 6, 4)
x2 <- c(10, 10, 20, 14, 2, 2, 40)
y <- c(10, 10, 10, 20)
w1 <- c(0.1, 0.1, 0.1, 0.7)

test_that("wtd.sd applies weights correctly", {
  expect_error(caretEnsemble:::wtd.sd(x))
  expect_false(sd(x1) == wtd.sd(x1, w = x2))
  expect_false(sd(x1) == wtd.sd(x1, w = x2))
  expect_equal(caretEnsemble:::wtd.sd(y, w = w1), 7.84, tolerance = .001)
  expect_equal(caretEnsemble:::wtd.sd(y, w = w1*100), caretEnsemble:::wtd.sd(y, w = w1))
})

test_that("wtd.sd handles NA values correctly", {
  y <- c(10, 10, 10, 20, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1)))
  expect_true(is.na(sd(y)))
  expect_true(!is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm=TRUE)))
  expect_true(!is.na(sd(y, na.rm=TRUE)))
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1)))
  expect_true(!is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm=TRUE)))
  w2 <- c(0.1, 0.1, NA, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm=TRUE) == caretEnsemble:::wtd.sd(y, w = w2, na.rm=TRUE)))
})

test_that("Checks generate errors", {
  skip_on_cran()
  skip_if_not_installed("rpart")
  set.seed(42)
  myControl <- trainControl(method="cv", number=5, savePredictions="final")
  expect_warning(
    x <- caretList(
      Sepal.Length ~ Sepal.Width,
      iris,
      methodList=c("glm", "lm"),
      trControl=myControl
    )
  )
  modelLibrary <- extractBestPreds(x)
  modelLibrary$nn <- modelLibrary$lm[sample(1:nrow(modelLibrary$lm), nrow(modelLibrary$lm)), ]

  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_warning(x$rpart <- train(Sepal.Length ~ Sepal.Width, iris, method="rpart"))
  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_error(check_caretList_classes(x$glm$finalModel))

  x$rpart <- train(Species ~ Sepal.Width, iris, method="rpart", trControl=myControl)
  # This has to be changed because train gives this error if dataset is truncated in
  # newest version of caret:
  # Error in train.default(x, y, weights = w, ...) :
  #    One or more factor levels in the outcome has no data: 'virginica'
  check_caretList_classes(x)
  expect_error(check_caretList_model_types(x))

  m <- extractBestPreds(x)
  expect_error(check_bestpreds_preds(m))

  set.seed(42)
  myControl2 <- trainControl(
    method="cv",
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
  expect_warning(
    x <- caretList(
      iris[1:100, -5],
      factor(ifelse(iris[1:100, "Species"] == "setosa", "Yes", "No")),
      methodList=c("lda", "rf"),
      trControl=myControl2
    )
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, iris, method="rpart")
  expect_error(check_caretList_model_types(x))
})
