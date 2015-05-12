# ## Test optimizers

set.seed(442)
library('caret')
library('rpart')
library('gbm')
library('kernlab')

train <- twoClassSim(
  n = 1000, intercept = -8, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(
  n = 1500, intercept = -7, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)

myList <- list(
  rf1=caretModelSpec(),
  rf2=caretModelSpec(method='rf', tuneLength=5),
  caretModelSpec(method='rpart'),
  caretModelSpec(method='knn', tuneLength=10),
  caretModelSpec(method = "glm")
)

myControl = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePredictions = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

context("Test optimizer passing to caretEnsemble correctly")

test_that("Test that optFUN does not take random values", {
  skip_on_cran()
  myCL <- caretList(
    x = train[, -23],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    tuneList = myList)
  expect_error(caretEnsemble(myCL, optFUN = randomAUC))
  expect_error(caretEnsemble(myCL, optFUN = noAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
                   caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))
})

context("Test more difficult cases")
load(system.file("testdata/studentEns.rda", package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/modeldat2.rda", package="caretEnsemble", mustWork=TRUE))
set.seed(3425)
ctrl <- trainControl(
  method = "cv",
  number = 5, classProbs = TRUE, savePredictions = TRUE,
  summaryFunction = twoClassSummary)

test_that("safe and greedy optimizers get same result in the limit", {
  skip_on_cran()

  myCL <- caretList(
    x = train[, -23],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    tuneList = myList)

  expect_identical(
    caretEnsemble(myCL, optFUN = safeOptAUC),
    caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(
    caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
    caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))
  expect_identical(
    caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
    caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(
    caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
    caretEnsemble(myCL, optFUN = greedOptAUC))
})

set.seed(3579)
ctrl <- trainControl(
  method = "cv",
  number = 5, classProbs = TRUE, savePredictions = TRUE,
  summaryFunction = twoClassSummary)
sampVec <- sample(1:151, 120)

test_that("Warnings and fallbacks in degenerate cases", {
  skip_on_cran()

  out <- caretList(
    x = modeldat2$traindata$preds[sampVec,],
    y = modeldat2$traindata$class[sampVec],
    trControl = ctrl,
    tuneLength = 3,
    methodList = c("knn", "nb", "lda"),
    tuneList = list(nnet=caretModelSpec(method='nnet', trace=FALSE))
  )

  predobs <- caretEnsemble:::makePredObsMatrix(out)

  wghts1 <- safeOptAUC(predobs$preds, predobs$obs)
  wghts2 <- greedOptAUC(predobs$preds, predobs$obs)

  expect_warning(safeOptAUC(predobs$preds, predobs$obs), "Returning best model")
  expect_message(greedOptAUC(predobs$preds, predobs$obs), "Try more iterations")
  expect_warning(caretEnsemble(out, optFUN = safeOptAUC), "Returning best model")
  expect_message(caretEnsemble(out, optFUN = greedOptAUC), "Try more iterations")

  expect_false(identical(wghts1, wghts2))
  expect_equal(wghts1, c(1, 0, 0, 0))
  expect_equal(wghts2, c(39, 1, 60, 0))

  ens1 <- caretEnsemble(out, optFUN = safeOptAUC)
  ens2 <- caretEnsemble(out, optFUN = greedOptAUC)
  expect_false(identical(ens1, ens2)) # Fixed
  expect_equivalent(ens1$weights, 1)
  expect_equivalent(length(ens1$weights), 1)
  expect_equivalent(ens2$weights, c(0.46, 0.54))
  expect_equivalent(length(ens2$weights), 2)
})

context("RMSE")
set.seed(87495)
load(system.file(
  "testdata/models_reg.rda",
  package="caretEnsemble", mustWork=TRUE))
load(system.file(
  "testdata/X.reg.rda",
  package="caretEnsemble", mustWork=TRUE))
load(system.file(
  "testdata/Y.reg.rda",
  package="caretEnsemble", mustWork=TRUE))

predobs <- caretEnsemble:::makePredObsMatrix(models_reg)

test_that("Test that optFUN does not take random values", {
  expect_error(caretEnsemble(models_reg, optFUN = randomRMSE))
  expect_error(caretEnsemble(models_reg, optFUN = noRMSE))
})

test_that("Optimizers respect iterations", {
  skip_on_cran()
  wghts1 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 3000)
  wghts2 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 20)
  wghts3 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 2)
  expect_false(identical(wghts1, wghts2))
  expect_false(identical(wghts2, wghts3))
  expect_identical(caretEnsemble(models_reg), caretEnsemble(models_reg, iter = 100))
  expect_false(identical(caretEnsemble(models_reg), caretEnsemble(models_reg, iter = 10)))
})

context("Test for NA value handling - classification")
set.seed(329)
myControl = trainControl(
  method = "cv", number = 10, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE,
  summaryFunction = twoClassSummary)
trainC <- twoClassSim(
  n = 2000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
  corrType = "AR1", corrValue = 0.6, mislabel = 0)
testC <- twoClassSim(
  n = 1000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
  corrType = "AR1", corrValue = 0.6, mislabel = 0)
MCAR.df <- function(df, p){
  MCARx <- function(x, p){
    z <- rbinom(length(x), 1, prob=p)
    x[z==1] <- NA
    return(x)
  }
  if(length(p) == 1){
    df <- apply(df, 2, MCARx, p)
  } else if(length(p) > 1) {
    df <- apply(df, 2, MCARx, sample(p, 1))
  }
  df <- as.data.frame(df)
  return(df)
}

test_that("Missing values are ignored in optimization", {
  skip_on_cran()
  set.seed(3256)
  trainC[, c(1:17)] <- MCAR.df(trainC[, c(1:17)], 0.15)
  testC[, c(1:17)] <- MCAR.df(testC[, c(1:17)], 0.05)

  set.seed(482)
  glm1 <- train(
    x = trainC[, c(1:17)], y = trainC[, "Class"], method = 'glm',
    trControl = myControl, metric = "ROC")
  set.seed(482)
  glm2 <- train(
    x = trainC[, c(1:17)], y = trainC[, "Class"], method = 'glm',
    trControl = myControl, preProcess = "medianImpute",
    metric = "ROC")
  set.seed(482)
  glm3 <- train(
    x = trainC[, c(2:9)], y = trainC[, "Class"], method = 'glm',
    trControl = myControl, metric = "ROC")
  set.seed(482)
  glm4 <- train(
    x = trainC[, c(1, 9:17)], y = trainC[, "Class"], method = 'glm',
    trControl = myControl, metric = "ROC")

  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- 'caretList'
  set.seed(482)

  predobs <- caretEnsemble:::makePredObsMatrix(nestedList)
  weights <- greedOptAUC(predobs$preds, predobs$obs)

  caretEnsemble:::getMetric.train(nestedList[[4]], "AUC")
  caTools:::colAUC(predobs$preds[,4], predobs$obs)
  caTools:::colAUC(predobs$preds[,2], predobs$obs)

  expect_equal(
    as.numeric(caTools:::colAUC(predobs$preds[,2], predobs$obs)),
    caretEnsemble:::getMetric.train(nestedList[[2]], "AUC"))
  expect_false(
    caTools:::colAUC(predobs$preds[,3], predobs$obs) ==
      caretEnsemble:::getMetric.train(nestedList[[3]], "AUC"))
  expect_false(
    caTools:::colAUC(predobs$preds[,4], predobs$obs) ==
      caretEnsemble:::getMetric.train(nestedList[[4]], "AUC"))
  expect_false(
    caTools:::colAUC(predobs$preds[,1], predobs$obs) ==
      caretEnsemble:::getMetric.train(nestedList[[1]], "AUC"))
})

context("Test for NA handling - regression")
test_that("Test for NA handling - regression", {
  skip_on_cran()
  myControl = trainControl(
    method = "cv", number = 10, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    returnResamp = "final",
    returnData = TRUE)
  trainC <- twoClassSim(
    n = 2000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
    corrType = "AR1", corrValue = 0.6, mislabel = 0)
  testC <- twoClassSim(
    n = 1000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
    corrType = "AR1", corrValue = 0.6, mislabel = 0)

  set.seed(3256)
  trainC[, c(1:15)] <- MCAR.df(trainC[, c(1:15)], 0.15)
  testC[, c(1:15)] <- MCAR.df(testC[, c(1:15)], 0.05)

  set.seed(482)
  glm1 <- train(
    x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = 'glm',
    trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm2 <- train(
    x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = 'glm',
    trControl = myControl, preProcess = "medianImpute", metric = "RMSE")
  set.seed(482)
  glm3 <- train(
    x = trainC[, c(2:9)], y = trainC[, "Corr2"], method = 'glm',
    trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm4 <- train(
    x = trainC[, c(1, 9:16)], y = trainC[, "Corr2"], method = 'glm',
    trControl = myControl, metric = "RMSE")

  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- 'caretList'

  set.seed(482)
  predobs <- caretEnsemble:::makePredObsMatrix(nestedList)
  weights <- greedOptRMSE(predobs$preds, predobs$obs)

  expect_equal(
    sqrt(mean((predobs$preds[,4] - predobs$obs) ^ 2L, na.rm=TRUE)),
    caretEnsemble:::getMetric.train(nestedList[[4]], "RMSE"))
  expect_equal(
    sqrt(mean((predobs$preds[,3] - predobs$obs) ^ 2L, na.rm=TRUE)),
    caretEnsemble:::getMetric.train(nestedList[[3]], "RMSE"))
  expect_equal(
    sqrt(mean((predobs$preds[,2] - predobs$obs) ^ 2L, na.rm=TRUE)),
    caretEnsemble:::getMetric.train(nestedList[[2]], "RMSE"))
  expect_equal(
    sqrt(mean((predobs$preds[,1] - predobs$obs) ^ 2L, na.rm=TRUE)),
    caretEnsemble:::getMetric.train(nestedList[[1]], "RMSE"))
})
