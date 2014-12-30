# ## Test optimizers

set.seed(442)
library('caret')
library('rpart')
library('gbm')
library('kernlab')

load(system.file("testdata/train.rda", package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/test.rda", package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/myList.rda", package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/myControl.rda", package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/myCL.rda", package="caretEnsemble", mustWork=TRUE))

context("Test optimizer passing to caretEnsemble correctly")

test_that("Test that optFUN does not take random values", {
  expect_error(caretEnsemble(myCL, optFUN = randomAUC))
  expect_error(caretEnsemble(myCL, optFUN = noAUC))
})

context("Test optimizers function similarly under normal conditions")

test_that("safe and greedy optimizers get same result in the limit", {
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC),
                    caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
                    caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))

})

context("Test more difficult cases")
test_that("Test more difficult cases", {
  skip_on_cran()

  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
                   caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
})

context("RMSE")
test_that('RMSE', {
  set.seed(87495)

  load(system.file("testdata/models_reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))

  predobs <- makePredObsMatrix(models_reg)

  expect_error(caretEnsemble(models_reg, optFUN = randomRMSE))
  expect_error(caretEnsemble(models_reg, optFUN = noRMSE))

  wghts1 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 3000)
  wghts2 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 20)
  wghts3 <- greedOptRMSE(predobs$preds, predobs$obs, iter = 2)
  expect_false(identical(wghts1, wghts2))
  expect_false(identical(wghts2, wghts3))
  expect_identical(caretEnsemble(models_reg), caretEnsemble(models_reg, iter = 100))
  expect_false(identical(caretEnsemble(models_reg), caretEnsemble(models_reg, iter = 10)))
})

context("Test for NA value handling - classification")
test_that('Test for NA value handling - classification', {

  mseeds <- vector(mode = "list", length = 12)
  for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
  mseeds[[12]] <- sample.int(1000, 1)
  myControl = trainControl(method = "cv", number = 10, repeats = 1,
                           p = 0.75, savePrediction = TRUE,
                           classProbs = TRUE, returnResamp = "final",
                           returnData = TRUE, seeds = mseeds,
                           summaryFunction = twoClassSummary)

  trainC <- twoClassSim(n = 2000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
                        corrType = "AR1", corrValue = 0.6, mislabel = 0)

  testC <- twoClassSim(n = 1000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
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

  set.seed(3256)
  trainC[, c(1:17)] <- MCAR.df(trainC[, c(1:17)], 0.15)
  testC[, c(1:17)] <- MCAR.df(testC[, c(1:17)], 0.05)

  set.seed(482)
  glm1 <- train(x = trainC[, c(1:17)], y = trainC[, "Class"], method = 'glm',
                trControl = myControl, metric = "ROC")
  set.seed(482)
  glm2 <- train(x = trainC[, c(1:17)], y = trainC[, "Class"], method = 'glm',
                trControl = myControl, preProcess = "medianImpute",
                metric = "ROC")
  set.seed(482)
  glm3 <- train(x = trainC[, c(2:9)], y = trainC[, "Class"], method = 'glm',
                trControl = myControl, metric = "ROC")
  set.seed(482)
  glm4 <- train(x = trainC[, c(1, 9:17)], y = trainC[, "Class"], method = 'glm',
                trControl = myControl, metric = "ROC")


  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- 'caretList'
  set.seed(482)

  predobs <- caretEnsemble:::makePredObsMatrix(nestedList)
  weights <- greedOptAUC(predobs$preds, predobs$obs)

  caretEnsemble:::getMetric.train(nestedList[[4]], "AUC")
  caTools:::colAUC(predobs$preds[,4], predobs$obs)
  caTools:::colAUC(predobs$preds[,2], predobs$obs)

  expect_equal(as.numeric(caTools:::colAUC(predobs$preds[,2], predobs$obs)),
               caretEnsemble:::getMetric.train(nestedList[[2]], "AUC"))
  expect_false(caTools:::colAUC(predobs$preds[,3], predobs$obs) ==
                 caretEnsemble:::getMetric.train(nestedList[[3]], "AUC"))
  expect_false(caTools:::colAUC(predobs$preds[,4], predobs$obs) ==
                 caretEnsemble:::getMetric.train(nestedList[[4]], "AUC"))
  expect_false(caTools:::colAUC(predobs$preds[,1], predobs$obs) ==
                 caretEnsemble:::getMetric.train(nestedList[[1]], "AUC"))
})

context("Test for NA handling - regression")
test_that('Test for NA handling - regression', {

  mseeds <- vector(mode = "list", length = 12)
  for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
  mseeds[[12]] <- sample.int(1000, 1)
  myControl = trainControl(method = "cv", number = 10, repeats = 1,
                           p = 0.75, savePrediction = TRUE,
                           returnResamp = "final",
                           returnData = TRUE, seeds = mseeds)

  #TODO: MOVE THIS TO INST/TESTDATA!
  trainC <- twoClassSim(n = 2000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
                        corrType = "AR1", corrValue = 0.6, mislabel = 0)

  testC <- twoClassSim(n = 1000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
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

  set.seed(3256)
  trainC[, c(1:15)] <- MCAR.df(trainC[, c(1:15)], 0.15)
  testC[, c(1:15)] <- MCAR.df(testC[, c(1:15)], 0.05)

  set.seed(482)
  glm1 <- train(x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = 'glm',
                trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm2 <- train(x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = 'glm',
                trControl = myControl, preProcess = "medianImpute", metric = "RMSE")
  set.seed(482)
  glm3 <- train(x = trainC[, c(2:9)], y = trainC[, "Corr2"], method = 'glm',
                trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm4 <- train(x = trainC[, c(1, 9:16)], y = trainC[, "Corr2"], method = 'glm',
                trControl = myControl, metric = "RMSE")

  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- 'caretList'

  set.seed(482)
  predobs <- caretEnsemble:::makePredObsMatrix(nestedList)
  weights <- greedOptRMSE(predobs$preds, predobs$obs)

  expect_equal(sqrt(mean((predobs$preds[,4] - predobs$obs) ^ 2L, na.rm=TRUE)),
               caretEnsemble:::getMetric.train(nestedList[[4]], "RMSE"))
  expect_equal(sqrt(mean((predobs$preds[,3] - predobs$obs) ^ 2L, na.rm=TRUE)),
               caretEnsemble:::getMetric.train(nestedList[[3]], "RMSE"))
  expect_equal(sqrt(mean((predobs$preds[,2] - predobs$obs) ^ 2L, na.rm=TRUE)),
               caretEnsemble:::getMetric.train(nestedList[[2]], "RMSE"))
  expect_equal(sqrt(mean((predobs$preds[,1] - predobs$obs) ^ 2L, na.rm=TRUE)),
               caretEnsemble:::getMetric.train(nestedList[[1]], "RMSE"))
})
