
########################################################################
context("Do the helper functions work for regression objects?")
########################################################################
library("caret")
library("randomForest")
library("rpart")

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

test_that("Recycling generates a warning", {
  expect_warning(wtd.sd(matrix(1:10, ncol=2),weights=1))
})

test_that("No predictions generates an error", {
  skip_if_not_installed('randomForest')
  skip_if_not_installed('gbm')
  skip_if_not_installed('plyr')
  skip_if_not_installed('glmnet')
  models_multi <- caretList(
    iris[,1:2], iris[,5],
    tuneLength=1, verbose=FALSE,
    methodList=c("rf", "gbm"),
    trControl=trainControl(method="cv", number=2, savePredictions=TRUE, classProbs=TRUE))
  expect_error(check_caretList_model_types(models_multi))

  models <- caretList(
    iris[,1:2], factor(ifelse(iris[,5]=="setosa", "Yes", "No")),
    tuneLength=1, verbose=FALSE,
    methodList=c("rf", "gbm"),
    trControl=trainControl(method="cv", number=2, savePredictions=TRUE, classProbs=TRUE))
  ctrl <- models[[1]]
  new_model <- train(
    iris[,1:2], factor(ifelse(iris[,5]=="setosa", "Yes", "No")),
    tuneLength=1,
    method=c("glmnet"),
    trControl=trainControl(method="cv", number=2, savePredictions=FALSE, classProbs=TRUE)
    )
  models2 <- c(list("glmnet"=new_model), models)
  models3 <- c(models, list("glmnet"=new_model))
  check_caretList_model_types(models)
  expect_error(check_caretList_model_types(models2))
  #expect_error(check_caretList_model_types(models3)) #THIS IS A BUG THAT NEEDS FIXING!!!!!!!!!!
})

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.reg)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 4)))
})

test_that("We can predict", {
  out <- predict(models.reg, "reg", newdata=X.reg)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 4)))
  expect_true(all(colnames(out)==c("rf", "lm", "glm", "knn")))
})

########################################################################
context("Do the helper functions work for classification objects?")
########################################################################

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.class)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 6)))
})

test_that("We can predict", {
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  out <- predict(models.class, "Classification", newdata=X.class)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 6)))
  expect_true(all(colnames(out)==c("rf", "glm", "svmRadial", "nnet", "treebag", "knn")))
  out2 <- predict(models.reg, "Regression", newdata = X.reg)
  expect_true(all(dim(out2)==c(150, 4)))
  expect_true(all(colnames(out2)==c("rf", "lm", "glm", "knn")))
})

test_that("predict results same regardless of verbose option", {
  expect_is(predict(models.class, "Classification", newdata = X.class), "matrix")
  out1 <- predict(models.class, "Classification", newdata = X.class)
  out2 <- predict(models.class, "Classification", verbose =TRUE, newdata = X.class)
  expect_identical(out1, out2)
  expect_is(predict(models.reg, "Regression", newdata = X.reg), "matrix")
  out1 <- predict(models.reg, "Regression", newdata =X.reg)
  out2 <- predict(models.reg, "Regression", verbose = TRUE, newdata = X.reg)
  expect_identical(out1, out2)
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

test_that("wtd.sd handles NA values correctly", {
  y <- c(10, 10, 10, 20, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  expect_true(is.na(wtd.sd(y)))
  expect_true(is.na(sd(y)))
  expect_true(!is.na(wtd.sd(y, na.rm=TRUE)))
  expect_true(!is.na(sd(y, na.rm=TRUE)))
  expect_true(is.na(wtd.sd(y, weights = w1)))
  expect_true(!is.na(wtd.sd(y, weights = w1, na.rm=TRUE)))
  w2 <- c(0.1, 0.1, NA, 0.7, NA, NA)
  expect_false(wtd.sd(y, weights = w1, na.rm=TRUE, normwt = TRUE) == wtd.sd(y, weights = w2, na.rm=TRUE, normwt = TRUE))
})

test_that("Checks generate errors", {
  skip_on_cran()
  skip_if_not_installed('rpart')
  set.seed(42)
  myControl <- trainControl(method="cv", number=5, savePredictions=TRUE)
  x <- caretList(
    Sepal.Length ~ Sepal.Width,
    head(iris, 100),
    methodList=c("glm", "lm"),
    trControl=myControl
  )
  modelLibrary <- extractBestPreds(x)
  modelLibrary$nn <- modelLibrary$lm[sample(1:nrow(modelLibrary$lm), nrow(modelLibrary$lm)),]

  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  x$rpart <- train(Sepal.Length ~ Sepal.Width, head(iris, 100), method="rpart")
  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_error(check_caretList_classes(x$glm$finalModel))

  x$rpart <- train(Species ~ Sepal.Width, head(iris, 100), method="rpart", trControl=myControl)
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
  x <- caretList(
    iris[1:100,-5],
    factor(ifelse(iris[1:100, "Species"] == "setosa", "Yes", "No")),
    methodList=c("lda", "rf"),
    trControl=myControl2
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, head(iris, 100), method="rpart")
  expect_error(check_caretList_model_types(x))
})
