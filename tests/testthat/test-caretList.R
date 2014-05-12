# Test caretList

set.seed(442)
library(caret)
library(randomForest)
train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3, 
                     noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3, 
                    noiseVars = 10, corrVars = 4, corrValue = 0.6)

#######################
# Classification models
########################

# Specify controls
myControl = trainControl(method = "cv", number = 3, repeats = 1, 
                         p = 0.75, savePrediction = TRUE, 
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE, returnResamp = "final", 
                         returnData = TRUE, verboseIter = FALSE)


# Simple two method list
test1 <- buildModels(methodList = c("knn", "glm"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"])


# Simple 4 method list
test2 <- buildModels(methodList = c("knn", "glm", "treebag"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], metric = "ROC")

test3 <- buildModels(methodList = c("pda", "lda2", "multinom", "bagFDA", "nnet", "gbm"), 
                     control = myControl, 
                     x = train[, -23], 
                     y = train[ , "Class"], metric = "ROC")

context("Test that buildModels makes model lists")
test_that("buildModels returns a list", {
  expect_is(test1, "list")
  expect_is(test2, "list")
  expect_is(test3, "list")
})

ens1 <- caretEnsemble(test1)
ens2 <- caretEnsemble(test2)
ens3 <- caretEnsemble(test3)


context("Test that buildModels can be ensembled")
test_that("buildModels objects can be ensembled", {
  expect_is(ens1, "caretEnsemble")
  expect_is(ens2, "caretEnsemble")
  expect_is(ens3, "caretEnsemble")
})


context("Test that buildModels perserves user specified functions")

myControl = trainControl(method = "cv", number = 3, repeats = 1, 
                         p = 0.75, savePrediction = TRUE, 
                         classProbs = TRUE, returnResamp = "final", 
                         returnData = TRUE, verboseIter = FALSE)


test2 <- buildModels(methodList = c("knn", "glm", "multinom"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], tuneLength = 4, baseSeed = 3252, 
                     metric = "Accuracy")

test1 <- buildModels(methodList = c("knn", "glm", "multinom"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], tuneLength = 7, baseSeed = 3252, 
                     metric = "Kappa")

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
tuneTest <- list(rf=list(tuneGrid=data.frame(.mtry=c(2,4,8,1))), 
                 nnet=list(tuneLength=3), 
                 knn=list(tuneLength=14))

# Simple with mix of data.frame and tuneLength

test2a <- buildModels(tuneList = tuneTest, control = myControl,  x = train[, -23], 
                     y = train[, "Class"])

myEns2a <- caretEnsemble(test2a)

test_that("User tuneTest parameters are respected and model is ensembled", {
  expect_is(myEns2a, "caretEnsemble")
  expect_is(test2a, "list")
  expect_equal(nrow(test2a[[1]]$results), 4)
  expect_equal(nrow(test2a[[2]]$results), 9)
  expect_equal(nrow(test2a[[3]]$results), 14)
})

context("More complex with multidimensional tuneGrid and NULL tuneLengths")

tuneTest2 <- list(glm = list(NULL), nnet = list(tuneLength = 4), 
                  knn = list(tuneLength = 2), 
                  avNNet = list(tuneGrid = data.frame(.size = c(1, 3, 5), 
                                                      .decay = c(0.8, 0.5, 0.2), 
                                                      .bag = c(1, 20, 40))))


test3a <- buildModels(tuneList = tuneTest2, control = myControl,  x = train[, -23], 
                      y = train[, "Class"])

myEns3a <- caretEnsemble(test3a)

test_that("User tuneTest parameters are respected and model is ensembled", {
  expect_is(myEns3a, "caretEnsemble")
  expect_is(test3a, "list")
  expect_equal(nrow(test3a[[1]]$results), 1)
  expect_equal(nrow(test3a[[2]]$results), 16)
  expect_equal(nrow(test3a[[3]]$results), 2)
  expect_equal(nrow(test3a[[3]]$results), 3)
})


rm(ens2, ens1, myEns1, myEns2, myEns2a, myEns3a, test1, test2, test2a, test3a, 
   myEns)

context("Test regression")
# Throws warning, but we're good

# Regression
myControl2 = trainControl(method = "cv", number = 3, repeats = 1, 
                          p = 0.75, savePrediction = TRUE, 
                          returnResamp = "final", 
                          returnData = TRUE, verboseIter = FALSE)

test1 <- buildModels(methodList = c("glm", "lm"), control = myControl2, 
                     x = train[, c(-23, -1)], 
                     y = train[, 1])


test2 <- buildModels(methodList = c("glm", "treebag", "nnet", "lm"), control = myControl2, 
                     x = train[, c(-23, -1)], 
                     y = train[, 1])

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

