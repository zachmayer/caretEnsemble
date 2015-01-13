# Test caretList

set.seed(442)
library('caret')
library('randomForest')
library('rpart')
library('gbm')
library('kernlab')
library('nnet')
library('ipred')
train <- twoClassSim(
  n = 1000, intercept = -8, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(
  n = 1500, intercept = -7, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)

###############################################
context("Ancillary caretList functions and errors")
################################################
test_that("caretModelSpec returns valid specs", {
  tuneList <- list(
    rf1=caretModelSpec(),
    rf2=caretModelSpec(method='rf', tuneLength=5),
    caretModelSpec(method='rpart'),
    caretModelSpec(method='knn', tuneLength=10)
  )
  tuneList <- tuneCheck(tuneList)
  expect_true(is.list(tuneList))
  expect_equal(length(tuneList), 4)
  expect_equal(sum(duplicated(names(tuneList))), 0)
})

###############################################
context("We can fit models with a mix of methodList and tuneList")
################################################
test_that("We can fit models with a mix of methodList and tuneList", {
  myList <- list(
    rpart=caretModelSpec(method='rpart', tuneLength=10),
    rf=caretModelSpec(method='rf', tuneGrid=data.frame(mtry=2))
  )
  expect_warning({
    test <- caretList(
      x = iris[,1:3],
      y = iris[,4],
      methodList = c("knn", "glm"),
      tuneList=myList
    )
  })
  expect_is(test, "caretList")
  expect_is(caretEnsemble(test), "caretEnsemble")
  expect_equal(length(test), 4)
  methods <- sapply(test, function(x) x$method)
  names(methods) <- NULL
  expect_equal(methods, c('rpart', 'rf', 'knn', 'glm'))
})

################################################
context("We can handle different CV methods")
################################################
test_that("We can handle different CV methods", {
  skip_on_cran()
  for(m in c(
    'boot',
    'adaptive_boot',
    'cv',
    'adaptive_cv',
    'LGOCV',
    'adaptive_LGOCV')
  ){
    test_name <- paste0('CV works with method=', m)
    test_that(test_name, {

      myControl = trainControl(
        method = m,
        number = 7,
        repeats = 1,
        p = 0.75,
        savePrediction = TRUE,
        returnResamp = "final",
        returnData = FALSE,
        verboseIter = FALSE)

      suppressWarnings({
        suppressMessages({
          models <- caretList(
            x = iris[,1:3],
            y = iris[,4],
            trControl = myControl,
            tuneLength=2,
            methodList = c('rpart', 'rf')
          )
        })
      })
      sink <- sapply(models, expect_is, class='train')

      suppressWarnings({
        suppressMessages({
          ens <- caretEnsemble(models, iter=10)
        })
      })

      expect_is(ens, 'caretEnsemble')

      suppressMessages({
        ens <- caretStack(models, method='glm', trControl=trainControl(number=2))
      })

      expect_is(ens, 'caretStack')
    })
  }
})

###############################################
context("Classification models")
################################################
test_that("Classification models", {
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
    test1 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("knn", "glm")
    )
  })

  expect_is(test1, "caretList")
  expect_is(caretEnsemble(test1), "caretEnsemble")
  expect_is(caretEnsemble(test1), "caretEnsemble")
})

test_that("Longer tests for Classification models", {
  skip_on_cran()
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
    test1 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("knn", "glm")
    )
  })

  expect_is(test1, "caretList")
  expect_is(caretEnsemble(test1), "caretEnsemble")
  expect_is(caretEnsemble(test1), "caretEnsemble")

  expect_warning({
    test2 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("knn", "glm", "rpart")
    )
  })

  expect_warning({
    test3 <- caretList(
      x = train[, -23],
      y = train[ , "Class"],
      metric = "ROC",
      trControl = myControl,
      methodList = c("svmLinear", "knn", "glm")
    )
  })

  expect_is(test2, "caretList")
  expect_is(test3, "caretList")
  expect_is(caretEnsemble(test2), "caretEnsemble")
  expect_is(caretEnsemble(test3), "caretEnsemble")

  expect_identical(test2[[1]]$metric, "ROC")
  expect_identical(test3[[1]]$metric, "ROC")
})

test_that("Test that caretList preserves user specified error functions", {
  skip_on_cran()

  myControl = trainControl(
    method = "cv", number = 3, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE)

  expect_warning({
    test1 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      tuneLength = 7,
      metric = "Kappa",
      trControl = myControl,
      methodList = c("knn", "rpart", "glm")
    )
  })

  expect_warning({
    test2 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      tuneLength = 4,
      metric = "Accuracy",
      trControl = myControl,
      methodList = c("knn", "rpart", "glm")
    )
  })

  expect_identical(test1[[1]]$metric, "Kappa")
  expect_identical(test2[[1]]$metric, "Accuracy")

  expect_equal(nrow(test1[[1]]$results), 7)
  expect_more_than(nrow(test1[[1]]$results), nrow(test2[[1]]$results))
  expect_equal(nrow(test2[[1]]$results), 4)

  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)
  expect_is(myEns2, "caretEnsemble")
  expect_is(myEns1, "caretEnsemble")
  myControl = trainControl(
    method = "cv", number = 3, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE)

  expect_warning({
    test1 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      tuneLength = 7,
      metric = "Kappa",
      trControl = myControl,
      methodList = c("knn", "rpart", "glm")
    )
  })

  expect_warning({
    test2 <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      tuneLength = 4,
      metric = "Accuracy",
      trControl = myControl,
      methodList = c("knn", "rpart", "glm")
    )
  })

  expect_identical(test1[[1]]$metric, "Kappa")
  expect_identical(test2[[1]]$metric, "Accuracy")

  expect_equal(nrow(test1[[1]]$results), 7)
  expect_more_than(nrow(test1[[1]]$results), nrow(test2[[1]]$results))
  expect_equal(nrow(test2[[1]]$results), 4)


  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)

  expect_is(myEns2, "caretEnsemble")
  expect_is(myEns1, "caretEnsemble")

})

test_that("Users can pass a custom tuneList", {
  skip_on_cran()
  # User specifies methods and tuning parameters specifically using a tuneList
  myControl = trainControl(
    method = "cv", number = 3, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE)

  tuneTest <- list(
    rpart=caretModelSpec(
      method='rpart',
      tuneGrid=data.frame(.cp=c(.01,.001,.1,1))
    ),
    knn=caretModelSpec(
      method='knn',
      tuneLength=9
    ),
    svmRadial=caretModelSpec(
      method='svmRadial',
      tuneLength=3
    ))

  expect_warning({
    test2a <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      trControl = myControl,
      tuneList = tuneTest
    )
  })

  myEns2a <- caretEnsemble(test2a)
  expect_is(myEns2a, "caretEnsemble")
  expect_is(test2a, "caretList")
  expect_equal(nrow(test2a[[1]]$results), 4)
  expect_equal(nrow(test2a[[2]]$results), 9)
  expect_equal(nrow(test2a[[3]]$results), 3)
})

context("User tuneTest parameters are respected and model is ensembled")
test_that("User tuneTest parameters are respected and model is ensembled", {
  skip_on_cran()

  myControl = trainControl(
    method = "cv", number = 3, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE)


  tuneTest <- list(
    nnet = caretModelSpec(
      method='nnet',
      tuneLength = 3,
      trace=FALSE,
      softmax=FALSE)
  )
  expect_warning({
    test <- caretList(
      x = train[, -23],
      y = train[, "Class"],
      trControl = myControl,
      tuneList = tuneTest
    )
  })
  ens <- caretEnsemble(test)
  expect_is(ens, "caretEnsemble")
  expect_is(test, "caretList")
  expect_equal(nrow(test[[1]]$results), 3*3)
  expect_false(test[[1]]$finalModel$softmax)
})

context("Formula interface for caretList works")
test_that("User tuneTest parameters are respected and model is ensembled", {
  skip_on_cran()
  tuneTest <- list(
    rpart = list(method='rpart', tuneLength = 2),
    nnet = list(method='nnet', tuneLength = 2, trace=FALSE),
    glm = list(method='glm')
  )
  x <- iris[,1:3]
  y <- iris[,4]
  expect_warning({
    set.seed(42)
    test_default <- caretList(
      x = x,
      y = y,
      tuneList = tuneTest
    )
  })
  expect_warning({
    set.seed(42)
    test_flma <- caretList(
      y ~ .,
      data = data.frame(y=y, x),
      tuneList = tuneTest
    )
  })
  ens_default <- caretEnsemble(test_default)
  ens_flma <- caretEnsemble(test_flma)
  expect_is(ens_default, "caretEnsemble")
  expect_is(ens_flma, "caretEnsemble")

  expect_equal(ens_default$RMSE, ens_flma$RMSE)
  expect_equal(ens_default$weights, ens_flma$weights)
})

###############################################
context("Regression models")
###############################################

test_that('Regression Models', {
  myControl2 = trainControl(
    method = "cv", number = 3, repeats = 1,
    p = 0.75, savePrediction = TRUE,
    returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE)

  expect_warning({
    test1 <- caretList(
      x = train[, c(-23, -1)],
      y = train[, 1],
      trControl = myControl2,
      methodList = c("glm", "lm")
    )
  })

  expect_warning({
    test2 <- caretList(
      x = train[, c(-23, -1)],
      y = train[, 1],
      trControl = myControl2,
      methodList = c("glm", "ppr", "lm")
    )
  })

  ens1 <- caretEnsemble(test1)
  ens2 <- caretEnsemble(test2)

  expect_is(test1, "caretList")
  expect_is(test2, "caretList")

  expect_is(ens1, "caretEnsemble")
  expect_is(ens2, "caretEnsemble")
})
