
context("Does variable importance work?")
library(caret)
library(randomForest)


load(system.file("testdata/models_reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/X.reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/Y.reg.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/models_class.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/X.class.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/Y.class.rda",
                 package="caretEnsemble", mustWork=TRUE))
load(system.file("testdata/stuGradMod.rda",
                 package="caretEnsemble", mustWork=TRUE))

set.seed(2239)
ens.class <- caretEnsemble(models_class, iter=1000)
# varImp struggles with the rf in our test suite, why?
ens.reg <- caretEnsemble(models_reg[2:4], iter=1000)

test_that("We can get variable importance in classification models", {
  expect_is(varImp(ens.class), "data.frame")
#   expect_is(varImp(ens.class, scale = FALSE), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("We can get variable importance in regression models", {
  expect_is(varImp(ens.reg), "data.frame")
#   expect_is(varImp(ens.reg, scale = FALSE), "data.frame")
  expect_is(varImp(ens.reg, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg, scale = TRUE, weight = TRUE), "data.frame")
})


test_that("We get warnings when scale is set to FALSE and weight is TRUE", {
  gives_warning(varImp(ens.reg, scale = FALSE, weight = TRUE))
  gives_warning(varImp(ens.class, scale = FALSE, weight = TRUE))
  expect_warning(varImp(ens.reg, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  gives_warning(varImp(ens.reg, scale = FALSE))
  gives_warning(varImp(ens.class, scale = FALSE))
  expect_warning(varImp(ens.reg, scale = FALSE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE),
                 "Weighting of unscaled")
})

ncol1 <- 7
ncol2 <- 4
nrow1 <- 6
nrow2 <- 6

test_that("We get the right dimensions back", {
  expect_equal(ncol(varImp(ens.class)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = FALSE)), ncol1-1)
  expect_equal(ncol(varImp(ens.class, weight = TRUE)), ncol1)
  expect_equal(ncol(varImp(ens.reg)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = FALSE)), ncol2-1)
  expect_equal(ncol(varImp(ens.reg, weight = TRUE)), ncol2)
  expect_equal(nrow(varImp(ens.class)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = FALSE)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = TRUE)), nrow1)
  expect_equal(nrow(varImp(ens.reg)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = FALSE)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = TRUE)), nrow2)
})


context("Do metric extraction functions work as expected")

test_that("Metric is used correctly", {
  expect_error(getRMSE(ens.class$models[[1]]))
  expect_error(getRMSE(ens.class$models[[3]]))
  expect_error(getMetric(ens.class$models[[3]], metric = "RMSE"))
  expect_error(getAUC(ens.reg$models[[1]]))
  expect_error(getAUC(ens.reg$models[[2]]))
  expect_error(getMetric(ens.reg$models[[2]], metric = "AUC"))
})

test_that("Metrics are accurate for AUC", {
  expect_equal(getAUC(ens.class$models[[1]]), 0.9257279, tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[2]]), 0.942959, tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[3]]), 0.9178055, tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[4]]), 0.9378095, tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[5]]), 0.9120618, tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[6]]), 0.9250347, tol = 0.0001)
})

test_that("getAUC and getMetric are identical", {
  expect_equal(getAUC(ens.class$models[[6]]),
               getMetric(ens.class$models[[6]], metric = "AUC"), tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[5]]),
               getMetric(ens.class$models[[5]], metric = "AUC"), tol = 0.0001)
  expect_equal(getAUC(ens.class$models[[4]]),
               getMetric(ens.class$models[[4]], metric = "AUC"), tol = 0.00001)
  expect_equal(getAUC(ens.class$models[[3]]),
               getMetric(ens.class$models[[3]], metric = "AUC"), tol = 0.00001)
  expect_equal(getAUC(ens.class$models[[2]]),
               getMetric(ens.class$models[[2]], metric = "AUC"), tol = 0.00001)
  expect_equal(getAUC(ens.class$models[[1]]),
               getMetric(ens.class$models[[1]], metric = "AUC"), tol = 0.00001)
})

test_that("Metrics are accurate for RMSE", {
  expect_equal(getRMSE(models_reg[[1]]), 0.3348216, tol = 0.0001)
  expect_equal(getRMSE(models_reg[[2]]), 0.324923, tol = 0.0001)
  expect_equal(getRMSE(models_reg[[3]]), 0.324923, tol = 0.0001)
  expect_equal(getRMSE(models_reg[[4]]), 0.3532128, tol = 0.0001)
})

test_that("getMetric and getRMSE are identical", {
  expect_equal(getRMSE(models_reg[[1]]),
               getMetric(models_reg[[1]], metric = "RMSE"), tol = 0.00001)
  expect_equal(getRMSE(models_reg[[2]]),
               getMetric(models_reg[[2]], metric = "RMSE"), tol = 0.00001)
  expect_equal(getRMSE(models_reg[[3]]),
               getMetric(models_reg[[3]], metric = "RMSE"), tol = 0.00001)
  expect_equal(getRMSE(models_reg[[4]]),
               getMetric(models_reg[[4]], metric = "RMSE"), tol = 0.00001)
})

test_that("getMetric fails when wrong metric is specificed", {
  expect_error(getMetric(models_reg[[1]], metric = "AUC"))
  expect_error(getMetric(models_reg[[2]], metric = "AUC"))
  expect_error(getMetric(models_reg[[3]], metric = "AUC"))
  expect_error(getMetric(models_reg[[4]], metric = "AUC"))
  expect_error(getMetric(models_class[[1]], metric = "RMSE"))
  expect_error(getMetric(models_class[[2]], metric = "RMSE"))
  expect_error(getMetric(models_class[[3]], metric = "RMSE"))
  expect_error(getMetric(models_class[[4]], metric = "RMSE"))
})


context("Metric Standard deviations are correct")

test_that("getMetricSD warnings are correct", {
  expect_warning(getMetricSD(models_reg[[1]]))
  expect_warning(getMetricSD(models_class[[1]]))
})

test_that("getMetricSD fails when wrong metric is specificed", {
  expect_error(getMetricSD(models_reg[[1]], metric = "AUC"))
  expect_error(getMetricSD(models_reg[[2]], metric = "AUC"))
  expect_error(getMetricSD(models_reg[[3]], metric = "AUC"))
  expect_error(getMetricSD(models_reg[[4]], metric = "AUC"))
  expect_error(getMetricSD(models_class[[1]], metric = "RMSE"))
  expect_error(getMetricSD(models_class[[2]], metric = "RMSE"))
  expect_error(getMetricSD(models_class[[3]], metric = "RMSE"))
  expect_error(getMetricSD(models_class[[4]], metric = "RMSE"))
})

test_that("getMetricSD works for RMSE", {
  expect_equal(getMetricSD(models_reg[[1]]), 0.06153832, tol = 0.00001)
  expect_equal(getMetricSD(models_reg[[2]]), 0.05517874, tol = 0.00001)
  expect_equal(getMetricSD(models_reg[[3]]), 0.05517874, tol = 0.00001)
  expect_equal(getMetricSD(models_reg[[4]]), 0.07114711, tol = 0.00001)
  expect_equal(getMetricSD(models_reg[[4]]),
               getMetricSD(models_reg[[4]], metric = "RMSE"))
  expect_equal(getMetricSD(models_reg[[3]]),
               getMetricSD(models_reg[[3]], metric = "RMSE"))
  expect_equal(getMetricSD(models_reg[[2]]),
               getMetricSD(models_reg[[2]], metric = "RMSE"))
  expect_equal(getMetricSD(models_reg[[1]]),
               getMetricSD(models_reg[[1]], metric = "RMSE"))
})

test_that("getMetricSD works for AUC", {
  expect_equal(getMetricSD(models_class[[1]]), 0.06200568, tol = 0.00001)
  expect_equal(getMetricSD(models_class[[2]]), 0.05196865, tol = 0.00001)
  expect_equal(getMetricSD(models_class[[3]]), 0.07725894, tol = 0.00001)
  expect_equal(getMetricSD(models_class[[4]]), 0.05947062, tol = 0.00001)
  expect_equal(getMetricSD(models_class[[4]]),
               getMetricSD(models_class[[4]], metric = "AUC"))
  expect_equal(getMetricSD(models_class[[3]]),
               getMetricSD(models_class[[3]], metric = "AUC"))
  expect_equal(getMetricSD(models_class[[2]]),
               getMetricSD(models_class[[2]], metric = "AUC"))
  expect_equal(getMetricSD(models_class[[1]]),
               getMetricSD(models_class[[1]], metric = "AUC"))
})

context("Metrics in student examples")

test_that("metrics work for AUC in imbalanced example", {
  expect_equal(getMetric(studentEns$models[[1]]), 0.8538603, tol = 0.00001)
  expect_equal(getMetric(studentEns$models[[2]]), 0.9238445, tol = 0.00001)
  expect_equal(getMetric(studentEns$models[[3]]), 0.9243697, tol = 0.00001)
  expect_equal(getMetric(studentEns$models[[4]]), 0.9624475, tol = 0.00001)
  expect_equal(getMetric(studentEns$models[[4]]),
               getMetric(studentEns$models[[4]], metric = "AUC"))
  expect_equal(getMetric(studentEns$models[[3]]),
               getMetric(studentEns$models[[3]], metric = "AUC"))
  expect_equal(getMetric(studentEns$models[[2]]),
               getMetric(studentEns$models[[2]], metric = "AUC"))
  expect_equal(getMetric(studentEns$models[[1]]),
               getMetric(studentEns$models[[1]], metric = "AUC"))
})

test_that("metric deviations work for AUC in imbalanced example", {
  expect_equal(getMetricSD(studentEns$models[[1]]), 0.1113234, tol = 0.00001)
  expect_equal(getMetricSD(studentEns$models[[2]]), 0.01121379, tol = 0.00001)
  expect_equal(getMetricSD(studentEns$models[[3]]), 0.05124492, tol = 0.00001)
  expect_equal(getMetricSD(studentEns$models[[4]]), 0.09980929, tol = 0.00001)
  expect_equal(getMetricSD(studentEns$models[[4]]),
               getMetricSD(studentEns$models[[4]], metric = "AUC"))
  expect_equal(getMetricSD(studentEns$models[[3]]),
               getMetricSD(studentEns$models[[3]], metric = "AUC"))
  expect_equal(getMetricSD(studentEns$models[[2]]),
               getMetricSD(studentEns$models[[2]], metric = "AUC"))
  expect_equal(getMetricSD(studentEns$models[[1]]),
               getMetricSD(studentEns$models[[1]], metric = "AUC"))
})

context("Does summary method work as expected")


test_that("No errors are thrown by a summary", {
  expect_output(summary(ens.class), "AUC")
  expect_output(summary(ens.reg), "RMSE")
  expect_output(summary(studentEns), "AUC")
})

context("Extract model Results")

extractModFrame



context("Plot.caretEnsemble")

test_that("Plot objects are ggplot", {
  expect_is(plot(ens.class), "ggplot")
  expect_is(plot(ens.reg), "ggplot")
  expect_is(plot(ens.reg$models[[2]]), "trellis")
})


test_that("Plot objects have proper data"){
  tp <- plot(ens.class)
  tp2 <- plot(ens.reg)
  expect_true(nrow(tp$data), 6)
  expect_true(nrow(tp2$data), 2)
  expect_identical(tp$data$method, names(ens.class$weights))
  expect_identical(tp2$data$method, names(ens.reg$weights))
}

context("fortify caretEnsemble")
fortify.caretEnsemble


context("autoplot caretEnsemble")


context("Residual extraction")
multiResiduals
residTest <- residuals(ens.class)
residTest2 <- residuals(ens.reg)
obs1 <- ifelse(Y.class == "No", 0, 1)
obs2 <- Y.reg
predTest <- predict(ens.class)
predTest2 <- predict(ens.reg)
identical(residTest, obs1 - predTest)
identical(residTest2, obs2 - predTest2)


test_that("Residuals provided by residuals are proper for ensemble objects", {
  expect_identical(residTest, obs1 - predTest)
  expect_identical(residTest2, obs2 - predTest2)
  expect_false(identical(residTest2, predTest2 -obs2))
  expect_false(identical(residTest, predTest -obs))
})

residuals.caretEnsemble

context("Does prediction method work for classification")

mseeds <- vector(mode = "list", length = 12)
for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
mseeds[[12]] <- sample.int(1000, 1)
myControl = trainControl(method = "cv", number = 10, repeats = 1,
                         p = 0.75, savePrediction = TRUE,
                         classProbs = TRUE, returnResamp = "final",
                         returnData = TRUE, seeds = mseeds)

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
              trControl = myControl)
set.seed(482)
glm2 <- train(x = trainC[, c(1:17)], y = trainC[, "Class"], method = 'glm',
              trControl = myControl, preProcess = "medianImpute")
set.seed(482)
glm3 <- train(x = trainC[, c(2:9)], y = trainC[, "Class"], method = 'glm',
              trControl = myControl)
set.seed(482)
glm4 <- train(x = trainC[, c(1, 9:17)], y = trainC[, "Class"], method = 'glm',
              trControl = myControl)



nestedList <- list(glm1, glm2, glm3, glm4)
set.seed(482)
ensNest <- caretEnsemble(nestedList, iter=2000)
pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)])
pred.nest1a <- predict(ensNest, newdata = testC[, c(1:17)])
pred.nest2 <- predict(ensNest, keepNA=FALSE, newdata = testC[, c(1:17)])
pred.nestTrain_a <- predict(ensNest, keepNA = FALSE)
pred.nestTrain_b <- predict(ensNest, keepNA = TRUE)

test_that("We can ensemble models and handle missingness across predictors", {
  expect_that(ensNest, is_a("caretEnsemble"))
  pred.nest1 <- predict(ensNest,  newdata=testC[, c(1:17)])
  expect_message(predict(ensNest, newdata=testC[, c(1:17)]))
  expect_message(predict(ensNest, keepNA=TRUE, newdata=testC[1:20, c(1:17)]))
  expect_true(is.numeric(pred.nest1))
  expect_true(is.list(pred.nest2))
  expect_true(is.numeric(pred.nest1a))
  expect_true(length(pred.nestTrain_b)==2000)
  expect_true(length(pred.nest1)==1000)
  expect_true(length(pred.nestTrain_a$predicted)==2000)
  expect_true(length(pred.nest2$predicted)==1000)
  expect_true(length(pred.nest1[is.na(pred.nest1)])>0)
})


## Test generics summary and predict

context("Test generic predict with errors")

pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)], se = TRUE)
pred.nest1a <- predict(ensNest, newdata = testC[, c(1:17)], se=TRUE)
pred.nest2 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE)
pred.nestTrain_a <- predict(ensNest, keepNA = FALSE, se =TRUE)


test_that("We can ensemble models and handle missingness across predictors", {
  expect_is(pred.nest1, "data.frame")
  expect_true(is.list(pred.nest2))
  expect_is(pred.nest1a, "data.frame")
  expect_is(pred.nestTrain_a, "list")
  expect_identical(names(pred.nest1), c("pred", "se"))
  expect_identical(names(pred.nest2), c("preds", "weight"))
  expect_identical(names(pred.nest2$preds), names(pred.nest1))
  expect_is(pred.nest2$weight, "matrix")
  expect_identical(pred.nest1, pred.nest1a)
  expect_true(length(pred.nest1)==2)
  expect_true(nrow(pred.nestTrain_a$preds)==2000)
  expect_true(nrow(pred.nest2$preds)==1000)
  expect_true(length(pred.nest1[is.na(pred.nest1)])>0)
})


context("Does prediction method work for regression")

mseeds <- vector(mode = "list", length = 12)
for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
mseeds[[12]] <- sample.int(1000, 1)
myControl = trainControl(method = "cv", number = 10, repeats = 1,
                         p = 0.75, savePrediction = TRUE,
                         returnResamp = "final",
                         returnData = TRUE, seeds = mseeds)

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
glm4 <- train(x = trainC[, c(1, 9:17)], y = trainC[, "Corr2"], method = 'glm',
              trControl = myControl, metric = "RMSE")



nestedList <- list(glm1, glm2, glm3, glm4)
set.seed(482)
ensNest <- caretEnsemble(nestedList, iter=2000)

pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:15)], se = TRUE)
pred.nest1a <- predict(ensNest, newdata = testC[, c(1:15)], se=TRUE)
pred.nest2 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:15)], se = TRUE)
pred.nestTrain_a <- predict(ensNest, keepNA = FALSE, se =TRUE)

test_that("We can ensemble models and handle missingness across predictors", {
  expect_is(pred.nest1, "data.frame")
  expect_true(is.list(pred.nest2))
  expect_is(pred.nest1a, "data.frame")
  expect_is(pred.nestTrain_a, "list")
  expect_identical(names(pred.nest1), c("pred", "se"))
  expect_identical(names(pred.nest2), c("preds", "weight"))
  expect_identical(names(pred.nest2$preds), names(pred.nest1))
  expect_is(pred.nest2$weight, "matrix")
  expect_identical(pred.nest1, pred.nest1a)
  expect_true(length(pred.nest1)==2)
  expect_true(nrow(pred.nestTrain_a$preds)==2000)
  expect_true(nrow(pred.nest2$preds)==1000)
  expect_true(length(pred.nest1$pred[is.na(pred.nest1$pred)]) == 0)
})

## Plot method
# plot(ens.class$models[[4]])






