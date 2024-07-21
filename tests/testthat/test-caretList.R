suppressMessages({
  library(testthat)
  library(caret)
  library(kernlab)
})

set.seed(442)

train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3, noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3, noiseVars = 10, corrVars = 4, corrValue = 0.6)

test_that("caretModelSpec generates valid specifications", {
  tuneList <- list(
    rf1 = caretModelSpec(),
    rf2 = caretModelSpec(method = "rf", tuneLength = 5),
    caretModelSpec(method = "rpart"),
    caretModelSpec(method = "knn", tuneLength = 10)
  )
  tuneList <- caretEnsemble:::tuneCheck(tuneList)

  expect_type(tuneList, "list")
  expect_equal(length(tuneList), 4)
  expect_equal(sum(duplicated(names(tuneList))), 0)

  all_models <- sort(unique(modelLookup()$model))
  for (model in all_models) {
    expect_equal(caretModelSpec(model, tuneLength = 5, preProcess = "knnImpute")$method, model)
  }
})

test_that("methodCheck and trControlCheck handle invalid inputs correctly", {
  expect_error(methodCheck(list(123)), "Method \"123\" is invalid.")
  expect_error(methodCheck(list("invalid_method")), "The following models are not valid caret models: invalid_method")

  ctrl <- trainControl(method = "cv", number = 5, savePredictions = c("all", "final"))
  expect_error(trControlCheck(ctrl, y = 1:10), "Please pass exactly 1 argument to savePredictions, e.g. savePredictions='final'")
})

test_that("caretList handles various model combinations correctly", {
  myControl2 <- trainControl(method = "cv", number = 3, p = 0.75, savePrediction = TRUE, returnResamp = "final", returnData = TRUE, verboseIter = FALSE)

  model_combinations <- list(
    list(c("glm", "lm"), 2),
    list(c("glm", "ppr", "lm"), 3),
    list(c("glm", "ppr", "lm", "rf"), 4)
  )

  for (combo in model_combinations) {
    methods <- combo[[1]]
    expected_length <- combo[[2]]

    suppressWarnings({
      test_models <- caretList(
        x = train[, c(-23, -1)],
        y = train[, 1],
        trControl = myControl2,
        methodList = methods
      )
    })

    expect_s3_class(test_models, "caretList")
    expect_equal(length(test_models), expected_length)

    suppressWarnings(ens <- caretEnsemble(test_models))
    expect_s3_class(ens, "caretEnsemble")
  }
})

test_that("caretList handles edge cases and errors gracefully", {
  myControl <- trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)

  expect_error(caretList(Sepal.Width ~ ., iris))
  expect_warning(caretList(Sepal.Width ~ ., iris, methodList = c("lm", "lm")))

  suppressWarnings({
    expect_s3_class(
      caretList(Sepal.Width ~ ., iris, methodList = "lm", continue_on_fail = TRUE),
      "caretList"
    )
  })

  bad_bad <- list(
    bad1 = caretModelSpec(method = "glm", tuneLength = 1),
    bad2 = caretModelSpec(method = "glm", tuneLength = 1)
  )
  good_bad <- list(
    good = caretModelSpec(method = "glmnet", tuneLength = 1),
    bad = caretModelSpec(method = "glm", tuneLength = 1)
  )

  expect_error(caretList(iris[, 1:4], iris[, 5], tuneList = bad_bad, trControl = myControl))
  expect_error(caretList(iris[, 1:4], iris[, 5], tuneList = good_bad, trControl = myControl))
  expect_error(caretList(iris[, 1:4], iris[, 5], tuneList = bad_bad, trControl = myControl, continue_on_fail = TRUE))

  suppressWarnings({
    working <- caretList(iris[, 1:4], iris[, 5], tuneList = good_bad, trControl = myControl, continue_on_fail = TRUE)
  })
  expect_s3_class(working, "caretList")
})

test_that("predict.caretList works correctly for various scenarios", {
  myControl <- trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)

  suppressWarnings({
    models <- caretList(
      iris[, 1:2], iris[, 5],
      tuneLength = 1, verbose = FALSE,
      methodList = "rf",
      tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE)),
      trControl = myControl
    )
  })

  suppressWarnings(p1 <- predict(models))
  p2 <- predict(models, newdata = iris[100, c(1:2)])
  p3 <- predict(models, newdata = iris[110, c(1:2)])

  expect_type(p1, "matrix")
  expect_type(p2, "matrix")
  expect_type(p3, "matrix")

  expect_equal(colnames(p1), names(models))
  expect_equal(colnames(p2), names(models))
  expect_equal(colnames(p3), names(models))

  # Test with probability predictions
  suppressWarnings({
    models_prob <- caretList(
      iris[, 1:2], iris[, 5],
      tuneLength = 1, verbose = FALSE,
      methodList = "rf",
      tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE)),
      trControl = trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
    )
  })

  suppressWarnings(p_prob <- predict(models_prob))
  expect_equal(ncol(p_prob), length(models_prob) * length(levels(iris[, 5])))

  # Test error handling
  models_prob[[1]]$modelType <- "Bogus"
  expect_error(suppressWarnings(predict(models_prob)))
})

test_that("caretList works with mixed methodList and tuneList", {
  skip_on_cran()
  myList <- list(
    rpart = caretModelSpec(method = "rpart", tuneLength = 10),
    rf = caretModelSpec(method = "rf", tuneGrid = data.frame(mtry = 2))
  )
  suppressWarnings({
    test <- caretList(
      x = iris[, 1:3],
      y = iris[, 4],
      methodList = c("knn", "glm"),
      tuneList = myList
    )
  })
  expect_s3_class(test, "caretList")
  expect_s3_class(caretEnsemble(test), "caretEnsemble")
  expect_equal(length(test), 4)
  methods <- sapply(test, function(x) x$method)
  expect_equal(sort(methods), sort(c("rpart", "rf", "knn", "glm")))
})

test_that("caretList handles different CV methods correctly", {
  skip_on_cran()
  cv_methods <- c("boot", "adaptive_boot", "cv", "repeatedcv", "adaptive_cv", "LGOCV", "adaptive_LGOCV")

  for (m in cv_methods) {
    myControl <- trainControl(
      method = m,
      number = 7,
      p = 0.75,
      savePredictions = "final",
      returnResamp = "final",
      returnData = FALSE,
      verboseIter = FALSE
    )

    suppressWarnings({
      suppressMessages({
        models <- caretList(
          x = iris[, 1:3],
          y = iris[, 4],
          trControl = myControl,
          tuneLength = 2,
          methodList = c("rpart", "rf")
        )
        ens <- caretStack(models, method = "glm", trControl = trainControl(number = 2))
      })
    })
    expect_true(all(sapply(models, inherits, "train")))
    expect_s3_class(caretEnsemble(models, trControl = trainControl(number = 2)), "caretEnsemble")
    expect_s3_class(ens, "caretStack")
  }
})

test_that("caretList fails gracefully for unsupported CV methods", {
  skip_on_cran()
  unsupported_methods <- c("boot632", "LOOCV", "none", "oob")

  for (m in unsupported_methods) {
    expect_s3_class(trainControl(method = m, index = 1:10, savePredictions = "final"), "list")
    expect_error(suppressWarnings(trControlCheck(trainControl(method = m, savePredictions = TRUE))))

    suppressWarnings(
      model <- train(
        Sepal.Length ~ Sepal.Width,
        tuneLength = 1,
        data = iris,
        method = ifelse(m == "oob", "rf", "lm"),
        trControl = trainControl(method = m, index = 1:10)
      )
    )
    expect_s3_class(model, "train")
  }
})

test_that("caretList works with custom models", {
  skip_on_cran()
  set.seed(1234)

  custom.rf <- getModelInfo("rf", regex = FALSE)[[1]]
  custom.rf$method <- "custom.rf"

  custom.rpart <- getModelInfo("rpart", regex = FALSE)[[1]]
  custom.rpart$method <- "custom.rpart"

  tune.list <- list(
    caretModelSpec(method = custom.rf, tuneLength = 1),
    myrpart = caretModelSpec(method = custom.rpart, tuneLength = 1),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1)
  )

  train.control <- trainControl(method = "cv", number = 2, classProbs = TRUE)
  X.df <- as.data.frame(iris[, -5])
  Y <- iris[, 5]

  expect_warning(cl <- caretList(X.df, Y, tuneList = tune.list, trControl = train.control))
  expect_s3_class(cl, "caretList")
  expect_silent(cs <- caretEnsemble(cl))
  expect_s3_class(cs, "caretEnsemble")

  expect_equal(sort(names(cs$models)), c("custom.rf", "myrpart", "treebag"))

  expect_warning(pred.classa <- predict(cs, type = "prob"))
  expect_silent(pred.classb <- predict(cs, newdata = X.df, type = "prob"))
  expect_silent(pred.classc <- predict(cs, newdata = X.df[2, , drop = FALSE], type = "prob"))

  expect_true(all(sapply(list(pred.classa, pred.classb, pred.classc), is.data.frame)))
  expect_equal(nrow(pred.classa), 150)
  expect_equal(nrow(pred.classb), 150)
  expect_equal(nrow(pred.classc), 1)
  expect_equal(pred.classa, pred.classb)
  expect_equal(pred.classc[1, 1], 0.9489, tolerance = 0.01)

  tune.list.no.method <- list(
    caretModelSpec(method = getModelInfo("rf", regex = FALSE)[[1]], tuneLength = 1),
    treebag = caretModelSpec(method = "treebag", tuneLength = 1)
  )
  expect_error(
    caretList(X.df, Y, tuneList = tune.list.no.method, trControl = train.control),
    "Custom models must be defined with a \"method\" attribute"
  )
})
