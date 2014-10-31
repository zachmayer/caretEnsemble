
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
ens.class <- caretEnsemble(models_class, iter=1000)
# varImp struggles with the rf in our test suite, why?
ens.reg <- caretEnsemble(models_reg[2:4], iter=1000)

test_that("We can get variable importance in classification models", {
  expect_is(varImp(ens.class), "data.frame")
  expect_is(varImp(ens.class, scale = FALSE), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("We can get variable importance in regression models", {
  expect_is(varImp(ens.reg), "data.frame")
  expect_is(varImp(ens.reg, scale = FALSE), "data.frame")
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


context("Does summary method work as expected")




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
# set.seed(482)
# glm5 <- train(x = trainC[, c(12:17)], y = trainC[, "Class"], method = 'glm',
#               trControl = myControl)


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
  expect_true(length(pred.nest2$predicted[is.na(pred.nest2$predicted)])!=0)
  expect_true(length(pred.nest1[is.na(pred.nest1)])>0)
})


## Test generics summary and predict

context("Test generic predict with errors")

pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)], se = TRUE)
pred.nest1a <- predict(ensNest, newdata = testC[, c(1:17)], se=TRUE)
pred.nest2 <- predict(ensNest, keepNA=FALSE, newdata = testC[, c(1:17)], se = TRUE)
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
  expect_true(sum(is.na(pred.nest2$preds$pred))!=0)
  expect_true(length(pred.nest1[is.na(pred.nest1)])>0)
})


context("Does prediction method work for regression")
