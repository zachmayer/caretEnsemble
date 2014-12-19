# Test caretList
library('caret')
library('randomForest')
library('rpart')
library('gbm')
library('kernlab')
library('nnet')
library('ipred')

###############################################
context("Ancillary caretList functions and errors")
################################################
test_that("caretModelSpec returns valid specs", {
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)
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
test_that("caretModelSpec returns valid specs", {
  skip_on_cran()
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)

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
context("We can work around bad models")
################################################

test_that("We can work around bad models", {
  skip_on_cran()
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)

  myList <- list(
    rpart=caretModelSpec(method='rpart', tuneLength=3),

    #Forgot to specify interaction.depth, shrinkage
    gbm=caretModelSpec(method='gbm', tuneGrid=data.frame(
      n.trees=-100,
      interaction.depth=-100,
      shrinkage=-100))
  )
  suppressWarnings({
    expect_error({
      test1 <- caretList(
        x = iris[,1:3],
        y = iris[,4],
        tuneList=myList,
        continue_on_model_fail=FALSE
      )
    })
    test <- caretList(
      x = iris[,1:3],
      y = iris[,4],
      tuneList=myList,
      continue_on_model_fail=TRUE
    )
  })

  expect_is(test, "caretList")
  expect_is(caretEnsemble(test), "caretEnsemble")
  expect_equal(length(test), 1)
  methods <- sapply(test, function(x) x$method)
  names(methods) <- NULL
  expect_equal(methods, c('rpart'))
})

################################################
context("We can handle different CV methods")
################################################

test_that('CV methods work', {
  skip_on_cran()
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)

  myControl = trainControl(
    method = 'cv',
    number = 7,
    repeats = 1,
    p = 0.75,
    savePrediction = TRUE,
    returnResamp = "final",
    returnData = FALSE,
    verboseIter = FALSE)

  for(m in c(
    'boot',
    'adaptive_boot',
    'cv',
    'adaptive_cv',
    'LGOCV',
    'adaptive_LGOCV')
  ){

    myControl$method <- m

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
  }
})

###############################################
context("Classification models")
################################################

test_that("Classification models", {
  rm(list=ls(all=TRUE))
  gc(reset=TRUE)
  load(system.file("testdata/train.rda", package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/test.rda", package="caretEnsemble", mustWork=TRUE))
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
