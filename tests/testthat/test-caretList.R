# Test caretList

set.seed(442)
library(caret)
library(randomForest)
train <- twoClassSim(
  n = 1000, intercept = -8, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(
  n = 1500, intercept = -7, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)

###############################################
context("Classification models")
################################################

# Specify controls
myControl = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

# Simple two method list
# Warning because we're going to auto-set indexes
expect_warning({
  test1 <- buildModels(
    x = train[, -23],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    methodList = c("knn", "glm")
  )
})

context("Test that buildModels makes model lists")
test_that("buildModels returns a list", {
  expect_is(test1, "list")
})

context("Test that buildModels can be ensembled")
test_that("buildModels objects can be ensembled", {
  expect_is(caretEnsemble(test1), "caretEnsemble")
})

context("Test that buildModels rejects bogus models")
test_that("buildModels objects can be ensembled", {
  expect_is(caretEnsemble(test1), "caretEnsemble")
})

context("Longer tests")
test_that("longer tests", {
  skip_on_cran()
  expect_warning({
    test2 <- buildModels(
      x = train[, -23],
      y = train[, "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("knn", "glm", "rpart")
    )
  })

  expect_warning({
    test3 <- buildModels(
      x = train[, -23],
      y = train[ , "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("svmLinear", "knn", "glm")
    )
  })

  expect_is(test2, "list")
  expect_is(test3, "list")
  expect_is(caretEnsemble(test2), "caretEnsemble")
  expect_is(caretEnsemble(test3), "caretEnsemble")

  test_that("buildModels objects preserve user metric", {
    expect_identical(test2[[1]]$metric, "ROC")
    expect_identical(test3[[1]]$metric, "ROC")
  })

})

context("Test that buildModels preserves user specified functions")

myControl = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

expect_warning({
  test1 <- buildModels(
    x = train[, -23],
    y = train[, "Class"],
    tuneLength = 7,
    metric = "Kappa",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
    )
})

expect_warning({
  test2 <- buildModels(
    x = train[, -23],
    y = train[, "Class"],
    tuneLength = 4,
    metric = "Accuracy",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
  )
})

test_that("buildModels objects preserve user metric", {
  expect_identical(test1[[1]]$metric, "Kappa")
  expect_identical(test2[[1]]$metric, "Accuracy")
  })

test_that("buildModels objects preserve user tuneLength", {
  expect_equal(nrow(test1[[1]]$results), 7)
  expect_more_than(nrow(test1[[1]]$results), nrow(test2[[1]]$results))
  expect_equal(nrow(test2[[1]]$results), 4)
})

myEns2 <- caretEnsemble(test2)
myEns1 <- caretEnsemble(test1)

test_that("User specified parameters can still be ensembled", {
  expect_is(myEns2, "caretEnsemble")
  expect_is(myEns1, "caretEnsemble")
})

context("Users can pass a custom tuneList")

# User specifies methods and tuning parameters specifically using a tuneList
tuneTest <- list(rpart=list(tuneGrid=data.frame(.cp=c(.01,.001,.1,1))),
                 knn=list(tuneLength=9),
                 svmRadial=list(
                   tuneGrid=data.frame(
                     .sigma = c(0.1, 0.2, 0.4),
                     .C=.1
                     )))

# Simple with mix of data.frame and tuneLength

test_that("User tuneTest parameters are respected and model is ensembled", {
  skip_on_cran()
  expect_warning({
    test2a <- buildModels(
      x = train[, -23],
      y = train[, "Class"],
      trControl = myControl,
      tuneList = tuneTest
    )
  })

  myEns2a <- caretEnsemble(test2a)
  expect_is(myEns2a, "caretEnsemble")
  expect_is(test2a, "list")
  expect_equal(nrow(test2a[[1]]$results), 4)
  expect_equal(nrow(test2a[[2]]$results), 9)
  expect_equal(nrow(test2a[[3]]$results), 3)
})

context("More complex with multidimensional tuneGrid and NULL tuneLengths")

context("User tuneTest parameters are respected and model is ensembled")
test_that("User tuneTest parameters are respected and model is ensembled", {
  skip_on_cran()
  tuneTest2 <- list(
    glm = list(tuneLength = 1),
    knn = list(tuneLength = 2),
    svmRadial = list(
      tuneGrid = expand.grid(
        .sigma = c(.005, .05, .5),
        .C = c(.001, .01, .1))))
  expect_warning({
    test3a <- buildModels(
      x = train[, -23],
      y = train[, "Class"],
      trControl = myControl,
      tuneList = tuneTest2
    )
  })
  myEns3a <- caretEnsemble(test3a)
  expect_is(myEns3a, "caretEnsemble")
  expect_is(test3a, "list")
  expect_equal(nrow(test3a[[1]]$results), 1)
  expect_equal(nrow(test3a[[2]]$results), 2)
  expect_equal(nrow(test3a[[3]]$results), 9)
})

###############################################
context("Regression models")
###############################################

myControl2 = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

expect_warning({
  test1 <- buildModels(
    x = train[, c(-23, -1)],
    y = train[, 1],
    trControl = myControl2,
    methodList = c("glm", "lm")
  )
})

expect_warning({
  test2 <- buildModels(
    x = train[, c(-23, -1)],
    y = train[, 1],
    trControl = myControl2,
    methodList = c("glm", "ppr", "lm")
  )
})

ens1 <- caretEnsemble(test1)
ens2 <- caretEnsemble(test2)

test_that("buildModels returns a list regression", {
  expect_is(test1, "list")
  expect_is(test2, "list")
})

test_that("buildModels objects can be ensembled regression", {
  expect_is(ens1, "caretEnsemble")
  expect_is(ens2, "caretEnsemble")
})
