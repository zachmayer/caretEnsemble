
context("Does ensembling and prediction work?")
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

test_that("We can ensemble regression models", {
  ens.reg <- caretEnsemble(models_reg, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})

test_that("We can ensemble classification models", {
  ens.class <- caretEnsemble(models_class, iter=1000)
  expect_that(ens.class, is_a("caretEnsemble"))
  pred.class <- predict(ens.class)
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)
})

context("Does ensembling work with nested models")

data(iris)
Y.reg <- iris[, 1]
X.reg <- model.matrix(~ ., iris[, -1])

mseeds <- vector(mode = "list", length = 12)
for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
mseeds[[12]] <- sample.int(1000, 1)
myControl = trainControl(method = "cv", number = 10, repeats = 1,
                         p = 0.75, savePrediction = TRUE,
                         classProbs = FALSE, returnResamp = "final",
                         returnData = TRUE, seeds = mseeds)


set.seed(482)
glm1 <- train(x = X.reg[, c(-1, -2, -6)], y = Y.reg, method = 'glm', trControl = myControl)
set.seed(482)
glm2 <- train(x = X.reg[, c(-1, -3, -6)], y = Y.reg, method = 'glm', trControl = myControl)
set.seed(482)
glm3 <- train(x = X.reg[, c(-1, -2, -3, -6)], y = Y.reg, method = 'glm', trControl = myControl)
set.seed(482)
glm4 <- train(x = X.reg[, c(-1, -4, -6)], y = Y.reg, method = 'glm', trControl = myControl)

nestedList <- list(glm1, glm2, glm3, glm4)

test_that("We can ensemble models of different predictors", {
ensNest <- caretEnsemble(nestedList, iter=1000)
expect_that(ensNest, is_a("caretEnsemble"))
pred.nest <- predict(ensNest, newdata = X.reg)
expect_true(is.numeric(pred.nest))
expect_true(length(pred.nest)==150)
})

context("Does ensembling work with missingness")

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
  expect_message(predict(ensNest, newdata = testC[, c(1:17)]))
  expect_message(predict(ensNest, keepNA=FALSE, newdata=testC[, c(1:17)]))
  expect_true(is.numeric(pred.nest1))
  expect_false(is.list(pred.nest2))
  expect_true(is.numeric(pred.nest2))
  expect_true(anyNA(pred.nest1))
  expect_false(anyNA(pred.nest2))
  expect_true(is.numeric(pred.nest1a))
  expect_true(length(pred.nestTrain_b)==2000)
  expect_true(length(pred.nest1)==1000)
  expect_true(length(pred.nestTrain_a)==2000)
  expect_true(length(pred.nest2)==1000)
})

test_that("Predictions the same for non-missing data under predict", {
  load(system.file("testdata/models_class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models_reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  ens.reg <- caretEnsemble(models_reg, iter=1000)
  ens.class <- caretEnsemble(models_class, iter=1000)
  pred1 <- predict(ens.reg, keepNA = FALSE)
  pred2 <- predict(ens.reg, keepNA = TRUE)
  expect_true(identical(pred1, pred2))
  pred1 <- predict(ens.class, keepNA = FALSE)
  pred2 <- predict(ens.class, keepNA = TRUE)
  expect_true(identical(pred1, pred2))
  pred1 <- predict(ens.reg, keepNA = FALSE, se = TRUE)
  pred2 <- predict(ens.reg, keepNA = TRUE, se = TRUE)
  expect_true(identical(pred1, pred2))
  pred1 <- predict(ens.class, keepNA = FALSE, se = TRUE)
  pred2 <- predict(ens.class, keepNA = TRUE, se =TRUE)
  expect_true(identical(pred1, pred2))
})

test_that("NA preservation and standard errors work right", {
  load(system.file("testdata/models_class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models_reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  ens.reg <- caretEnsemble(models_reg, iter=1000)
  ens.class <- caretEnsemble(models_class, iter=1000)
  pred1 <- predict(ens.reg, keepNA = FALSE)
  pred2 <- predict(ens.reg, keepNA = TRUE)
  expect_is(pred1, "numeric")
  expect_is(pred2, "numeric")
  pred1 <- predict(ens.class, keepNA = FALSE)
  pred2 <- predict(ens.class, keepNA = TRUE)
  expect_is(pred1, "numeric")
  expect_is(pred2, "numeric")
  pred1 <- predict(ens.class, keepNA = FALSE, se = TRUE)
  pred2 <- predict(ens.class, keepNA = TRUE, se = TRUE)
  expect_is(pred1, "data.frame")
  expect_is(pred2, "data.frame")
  pred1 <- predict(ens.reg, keepNA = FALSE, se = TRUE)
  pred2 <- predict(ens.reg, keepNA = TRUE, se = TRUE)
  expect_is(pred1, "data.frame")
  expect_is(pred2, "data.frame")
  nestedList <- list(glm1, glm2, glm3, glm4)
  ensNest <- caretEnsemble(nestedList, iter=2000)
  pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)], se =TRUE)
  pred.nest1a <- predict(ensNest, newdata = testC[, c(1:17)], se = TRUE)
  pred.nest2 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE)
  pred.nestTrain_a <- predict(ensNest, keepNA = FALSE, se = TRUE)
  pred.nestTrain_b <- predict(ensNest, keepNA = TRUE, se = TRUE)
  expect_is(pred.nest1, "data.frame")
  expect_is(pred.nest1a, "data.frame")
  expect_is(pred.nest2, "data.frame")
  expect_is(pred.nestTrain_a, "data.frame")
  expect_is(pred.nestTrain_b, "data.frame")
})


test_that("Messages appear in predict only when missing values are there", {
  expect_message(predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)]), "complete data")
  expect_message(predict(ensNest, newdata = testC[, c(1:17)]), "complete data")
  expect_message(predict(ensNest, keepNA=FALSE, newdata = testC[, c(1:17)]), "available data")
  expect_message(predict(ensNest, keepNA = FALSE), "available data")
  expect_message(predict(ensNest, keepNA = TRUE), "complete data")
  expect_warning(predict(ensNest, keepNA = TRUE, return_weights = "car"), "default set to")
})


test_that("Predict respects user return_weights options", {
  nestedList <- list(glm1, glm2, glm3, glm4)
  set.seed(482)
  ensNest <- caretEnsemble(nestedList, iter=2000)
  pred.nest1 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE)
  pred.nest2 <- predict(ensNest, keepNA = FALSE, se = TRUE)
  pred.nest3 <- predict(ensNest, keepNA = TRUE, se = TRUE)
  pred.nest4 <- predict(ensNest, keepNA = TRUE, se = FALSE)
  expect_is(pred.nest1, "data.frame")
  expect_is(pred.nest2, "data.frame")
  expect_is(pred.nest3, "data.frame")
  expect_is(pred.nest4, "numeric")
  expect_identical(names(pred.nest1), names(pred.nest2))
  expect_identical(names(pred.nest2), names(pred.nest3))
  pred.nest1 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE,
                        return_weights = TRUE)
  pred.nest2 <- predict(ensNest, keepNA = FALSE, se = TRUE, return_weights = TRUE)
  pred.nest3 <- predict(ensNest, keepNA = TRUE, se = TRUE, return_weights = TRUE)
  pred.nest4 <- predict(ensNest, keepNA = TRUE, se = FALSE, return_weights = TRUE)
  pred.nest5 <- predict(ensNest, keepNA = FALSE, se = FALSE, return_weights = TRUE)
  expect_is(pred.nest1, "list")
  expect_is(pred.nest2, "list")
  expect_is(pred.nest3, "list")
  expect_is(pred.nest4, "list")
  expect_is(pred.nest5, "list")
})

test_that("Predict lists have proper structure", {
  pred.nest1 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE,
                        return_weights = TRUE)
  pred.nest2 <- predict(ensNest, keepNA = FALSE, se = TRUE, return_weights = TRUE)
  pred.nest3 <- predict(ensNest, keepNA = TRUE, se = TRUE, return_weights = TRUE)
  pred.nest4 <- predict(ensNest, keepNA = TRUE, se = FALSE, return_weights = TRUE)
  pred.nest5 <- predict(ensNest, keepNA = FALSE, se = FALSE, return_weights = TRUE)
  expect_identical(names(pred.nest1), c("preds", "weight"))
  expect_identical(names(pred.nest1), names(pred.nest2))
  expect_identical(names(pred.nest1), names(pred.nest3))
  expect_identical(names(pred.nest1), names(pred.nest4))
  expect_identical(names(pred.nest1), names(pred.nest5))
  expect_identical(names(pred.nest1$preds), names(pred.nest2$preds))
  expect_identical(names(pred.nest3$preds), names(pred.nest2$preds))
  expect_null(names(pred.nest4$preds))
  expect_null(names(pred.nest5$preds))
  expect_identical(nrow(pred.nest1$preds), 1000L)
  expect_identical(nrow(pred.nest2$preds), nrow(pred.nest3$preds))
  expect_is(pred.nest1$weight, "matrix")
  expect_is(pred.nest2$weight, "matrix")
  expect_is(pred.nest3$weight, "matrix")
  expect_is(pred.nest4$weight, "matrix")
  expect_is(pred.nest5$weight, "matrix")
  expect_identical(dim(pred.nest1$weight), c(1000L, 4L))
  expect_identical(dim(pred.nest2$weight), c(2000L, 4L))
  expect_identical(dim(pred.nest3$weight), c(1L, 4L))
  expect_identical(dim(pred.nest4$weight), c(1L, 4L))
  expect_identical(dim(pred.nest5$weight), c(2000L, 4L))
  #TODO: Fix names on the weight matrix returned here
})
