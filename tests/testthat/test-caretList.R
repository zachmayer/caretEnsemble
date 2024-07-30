# Test caretList
set.seed(442L)
suppressMessages({
  library(testthat)
  library(caret)
  library(kernlab)
})
train <- caret::twoClassSim(
  n = 1000L, intercept = -8L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)
test <- caret::twoClassSim(
  n = 1500L, intercept = -7L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)

###############################################
context("Ancillary caretList functions and errors")
################################################
test_that("caretModelSpec returns valid specs", {
  tuneList <- list(
    rf1 = caretModelSpec(),
    rf2 = caretModelSpec(method = "rf", tuneLength = 5L),
    caretModelSpec(method = "rpart"),
    caretModelSpec(method = "knn", tuneLength = 10L)
  )
  tuneList <- caretEnsemble::tuneCheck(tuneList)
  expect_type(tuneList, "list")
  expect_length(tuneList, 4L)
  expect_equal(sum(duplicated(names(tuneList))), 0L)
})

test_that("caretModelSpec and checking functions work as expected", {
  all_models <- sort(unique(modelLookup()$model))
  for (model in all_models) {
    expect_equal(caretModelSpec(model, tuneLength = 5L, preProcess = "knnImpute")$method, model)
  }

  tuneList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  all_models_check <- tuneCheck(tuneList)
  expect_is(all_models_check, "list")
  expect_length(all_models, length(all_models_check))

  tuneList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  names(tuneList) <- all_models
  names(tuneList)[c(1L, 5L, 10L)] <- ""
  all_models_check <- tuneCheck(tuneList)
  expect_is(all_models_check, "list")
  expect_length(all_models, length(all_models_check))

  methodCheck(all_models)
  expect_error(methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL")))
  expect_error(methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL", "GBM")))
})

test_that("Target extraction functions work", {
  data(iris)
  expect_equal(extractCaretTarget(iris[, 1L:4L], iris[, 5L]), iris[, 5L])
  expect_equal(extractCaretTarget(iris[, 2L:5L], iris[, 1L]), iris[, 1L])
  expect_equal(extractCaretTarget(Species ~ ., iris), iris[, "Species"])
  expect_equal(extractCaretTarget(Sepal.Width ~ ., iris), iris[, "Sepal.Width"])
})

test_that("caretList errors for bad models", {
  data(iris)

  # Basic checks
  expect_error(caretList(Sepal.Width ~ ., iris), "Please either define a methodList or tuneList")
  expect_warning(
    caretList(Sepal.Width ~ ., iris, methodList = c("lm", "lm")),
    "Duplicate entries in methodList. Using unqiue methodList values."
  )
  expect_is(caretList(Sepal.Width ~ ., iris, methodList = "lm", continue_on_fail = TRUE), "caretList")

  # Check that by default a bad model kills the training job
  my_control <- trainControl(method = "cv", number = 2L, classProbs = TRUE)
  bad <- list(
    bad = caretModelSpec(method = "glm", tuneLength = 1L)
  )
  expect_output(
    expect_warning(
      expect_error(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = bad, trControl = my_control),
        regexp = "Stopping" # From caret.train
      ),
      regexp = "model fit failed for Fold1"
    ),
    regexp = "Something is wrong; all the Accuracy metric values are missing:"
  )
  expect_output(
    expect_warning(
      expect_error(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = bad, trControl = my_control, continue_on_fail = TRUE),
        regexp = "caret:train failed for all models. Please inspect your data."
      ),
      regexp = "model fit failed for Fold1" # From caretList
    ),
    regexp = "Something is wrong; all the Accuracy metric values are missing:"
  )

  # Check that at least one good model + continue_on_fail works
  good_bad <- list(
    good = caretModelSpec(method = "glmnet", tuneLength = 1L),
    bad = caretModelSpec(method = "glm", tuneLength = 1L)
  )
  expect_s3_class(
    expect_output(
      expect_warning(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = good_bad, trControl = my_control, continue_on_fail = TRUE),
        regexp = "model fit failed for Fold1" # From caretList
      ),
      regexp = "Something is wrong; all the Accuracy metric values are missing:"
    ), "caretList"
  )
})

test_that("caretList predictions", {
  expect_warning(
    models <- caretList(
      iris[, 1L:2L], iris[, 3L],
      tuneLength = 1L, verbose = FALSE,
      methodList = "rf", tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE)),
      trControl = trainControl(
        method = "cv",
        number = 2L, savePredictions = "final"
      )
    ), "There were missing values in resampled performance measures."
  )

  p1 <- predict(models)
  p2 <- predict(models, newdata = iris[100L, 1L:2L])
  p3 <- predict(models, newdata = iris[110L, 1L:2L])
  expect_is(p1, "data.table")
  expect_is(p1[[1L]], "numeric")
  expect_is(p1[[2L]], "numeric")
  expect_named(models, colnames(p1))
  expect_is(p2, "data.table")
  expect_is(p2[[1L]], "numeric")
  expect_is(p2[[2L]], "numeric")
  expect_named(models, colnames(p2))
  expect_is(p3, "data.table")
  expect_is(p3[[1L]], "numeric")
  expect_is(p3[[2L]], "numeric")
  expect_named(models, colnames(p3))

  models <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = "rf",
    tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE)),
    trControl = trainControl(
      method = "cv", number = 2L, savePredictions = "final", classProbs = TRUE
    )
  )

  p2 <- predict(models, excluded_class_id = 0L)
  p3 <- predict(models, newdata = iris[, 1L:2L], excluded_class_id = 0L)
  expect_is(p2, "data.table")
  expect_is(p2[[1L]], "numeric")
  expect_is(p2[[2L]], "numeric")
  expect_is(p2[[3L]], "numeric")
  expect_is(p2[[4L]], "numeric")
  expect_is(p3, "data.table")
  expect_is(p3[[1L]], "numeric")
  expect_is(p3[[2L]], "numeric")
  expect_is(p3[[3L]], "numeric")
  expect_is(p3[[4L]], "numeric")
  expect_equal(
    length(names(models)) * nlevels(as.factor(iris[, 5L])),
    length(colnames(p3))
  ) # check that we have the right number of columns
  expect_identical(dim(p2), dim(p3))
  expect_named(p2, names(p3))

  modelnames <- names(models)
  classes <- levels(iris[, 5L])
  combinations <- expand.grid(classes, modelnames)
  correct_colnames <- apply(combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  expect_equal(
    correct_colnames,
    colnames(p3)
  ) # check the column names are correct and ordered correctly (methodname_classname)

  models[[1L]]$modelType <- "Bogus"
  expect_error(predict(models))
})

test_that("as.caretList.list returns a caretList object", {
  modelList <- caretList(Sepal.Length ~ Sepal.Width,
    head(iris, 50L),
    methodList = c("glm", "lm", "knn")
  )
  class(modelList) <- "list"
  expect_is(as.caretList(modelList), "caretList")
})

#############################################################
context("Bad characters in target variable names and model names")
#############################################################
test_that("Target variable names with character | are not allowed", {
  bad_iris <- iris[1L:100L, ]
  bad_iris[, 5L] <- gsub("versicolor", "versicolor|1", bad_iris[, 5L], fixed = TRUE)
  bad_iris[, 5L] <- gsub("setosa", "setosa|2", bad_iris[, 5L], fixed = TRUE)
  bad_iris[, 5L] <- as.factor(as.character(bad_iris[, 5L]))

  # Expect an error from caret
  expect_error(model_list <- caretList(
    x = bad_iris[, -5L],
    y = bad_iris[, 5L],
    methodList = c("rpart", "glmnet"),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      classProbs = TRUE,
      savePredictions = "final",
      index = createFolds(bad_iris$Species, 2L, list = TRUE, returnTrain = TRUE)
    )
  ))
})

test_that("Character | in model names is transformed into a point", {
  reduced_iris <- iris[1L:100L, ]
  reduced_iris[, 5L] <- as.factor(as.character(reduced_iris[, 5L]))

  # Chack that specified model names are transformed with function make.names
  model_list <- caretList(
    x = reduced_iris[, -5L],
    y = reduced_iris[, 5L],
    tuneList = list(
      "nnet|1" = caretModelSpec(
        method = "nnet",
        tuneGrid = expand.grid(.size = c(1L, 3L), .decay = 0.3),
        trace = FALSE
      ),
      "nnet|2" = caretModelSpec(
        method = "nnet",
        tuneGrid = expand.grid(.size = 3L, .decay = c(0.1, 0.3)),
        trace = FALSE
      )
    ),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      classProbs = TRUE,
      savePredictions = "final",
      summaryFunction = twoClassSummary,
      index = createFolds(reduced_iris$Species, 2L, list = TRUE, returnTrain = TRUE)
    )
  )
  expect_named(model_list, c("nnet.1", "nnet.2"))
})

###############################################
context("We can fit models with a mix of methodList and tuneList")
################################################
test_that("We can fit models with a mix of methodList and tuneList", {
  skip_on_cran()
  myList <- list(
    rpart = caretModelSpec(method = "rpart", tuneLength = 10L),
    rf = caretModelSpec(method = "rf", tuneGrid = data.frame(mtry = 2L))
  )
  expect_warning(
    test <- caretList(
      x = iris[, 1L:3L],
      y = iris[, 4L],
      methodList = c("knn", "glm"),
      tuneList = myList
    ), "There were missing values in resampled performance measures."
  )
  expect_is(test, "caretList")
  expect_is(caretEnsemble(test), "caretEnsemble")
  expect_length(test, 4L)
  methods <- sapply(test, function(x) x$method)
  names(methods) <- NULL
  expect_equal(methods, c("rpart", "rf", "knn", "glm"))
})

################################################
context("We can handle different CV methods")
################################################
test_that("We can handle different CV methods", {
  skip_on_cran()
  for (m in c(
    "boot",
    "adaptive_boot",
    "cv",
    "repeatedcv",
    "adaptive_cv",
    "LGOCV",
    "adaptive_LGOCV"
  )
  ) {
    N <- 7L
    x <- iris[, 1L:3L]
    y <- iris[, 4L]

    if (m == "boot" || m == "adaptive_boot") {
      idx <- caret::createResample(y, times = N, list = TRUE)
    } else if (m == "cv" || m == "adaptive_cv") {
      idx <- caret::createFolds(y, k = N, list = TRUE, returnTrain = TRUE)
    } else if (m == "repeatedcv") {
      idx <- caret::createMultiFolds(y, k = N, times = 2L)
    } else if (m == "LGOCV" || m == "adaptive_LGOCV") {
      idx <- caret::createDataPartition(
        y,
        times = N,
        p = 0.5,
        list = TRUE,
        groups = min(5L, length(y))
      )
    }

    myControl <- trainControl(
      method = m,
      number = N,
      p = 0.75,
      savePredictions = "final",
      returnResamp = "final",
      returnData = FALSE,
      verboseIter = FALSE,
      index = idx
    )

    expect_warning(
      models <- caretList(
        x = x,
        y = y,
        trControl = myControl,
        tuneLength = 2L,
        methodList = c("rpart", "rf")
      ), "There were missing values in resampled performance measures."
    )
    ens <- caretStack(models, method = "glm", trControl = trainControl(method = "cv", number = 2L))

    invisible(sapply(models, expect_is, class = "train"))

    ens <- caretEnsemble(models, trControl = trainControl(method = "cv", number = 2L))

    expect_is(ens, "caretEnsemble")

    ens <- caretStack(models, method = "glm", trControl = trainControl(method = "cv", number = 2L))

    expect_is(ens, "caretStack")
  }
})

test_that("Non standard cv methods work", {
  data(iris)
  for (m in c(
    "boot632",
    "LOOCV",
    "none",
    "oob"
  )
  ) {
    # Ignore method if index is specified
    expect_is(trainControl(method = m, index = 1L:10L, savePredictions = "final"), "list")

    # Model itself should fit fine
    model <- train(
      Sepal.Length ~ Sepal.Width,
      tuneLength = 1L,
      data = iris,
      method = "rf",
      trControl = trainControl(
        method = m,
        returnResamp = "final",
        index = caret::createFolds(iris$Species, 2L, list = TRUE, returnTrain = TRUE)
      )
    )
    expect_is(model, "train")
  }
})

###############################################
context("Classification models")
################################################
test_that("Classification models", {
  # Specify controls
  myControl <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePredictions = "final",
    summaryFunction = twoClassSummary,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE
  )

  # Simple two method list
  # Warning because we Are going to auto-set indexes
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    methodList = c("knn", "glm")
  )

  expect_is(test1, "caretList")
  expect_is(caretEnsemble(test1), "caretEnsemble")
  expect_is(caretEnsemble(test1), "caretEnsemble")
})

test_that("Longer tests for Classification models", {
  skip_on_cran()
  # Specify controls
  myControl <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePredictions = "final",
    summaryFunction = twoClassSummary,
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE
  )

  # Simple two method list
  # Warning because we Are going to auto-set indexes
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    methodList = c("knn", "glm")
  )

  expect_is(test1, "caretList")
  expect_is(caretEnsemble(test1), "caretEnsemble")
  expect_is(caretEnsemble(test1), "caretEnsemble")

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    methodList = c("knn", "glm", "rpart")
  )

  test3 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    trControl = myControl,
    methodList = c("svmLinear", "knn", "glm")
  )

  expect_is(test2, "caretList")
  expect_is(test3, "caretList")
  expect_is(caretEnsemble(test2), "caretEnsemble")
  expect_is(caretEnsemble(test3), "caretEnsemble")

  expect_identical(test2[[1L]]$metric, "ROC")
  expect_identical(test3[[1L]]$metric, "ROC")
})

test_that("Test that caretList preserves user specified error functions", {
  skip_on_cran()
  myControl <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePredictions = "final",
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE
  )

  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 7L,
    metric = "Kappa",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
  )

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 4L,
    metric = "Accuracy",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
  )

  expect_identical(test1[[1L]]$metric, "Kappa")
  expect_identical(test2[[1L]]$metric, "Accuracy")

  expect_equal(nrow(test1[[1L]]$results), 7L)
  expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  expect_equal(nrow(test2[[1L]]$results), 4L)

  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)
  expect_is(myEns2, "caretEnsemble")
  expect_is(myEns1, "caretEnsemble")
  myControl <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePredictions = "final",
    classProbs = TRUE, returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE
  )

  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 7L,
    metric = "Kappa",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
  )

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 4L,
    metric = "Accuracy",
    trControl = myControl,
    methodList = c("knn", "rpart", "glm")
  )

  expect_identical(test1[[1L]]$metric, "Kappa")
  expect_identical(test2[[1L]]$metric, "Accuracy")

  expect_equal(nrow(test1[[1L]]$results), 7L)
  expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  expect_equal(nrow(test2[[1L]]$results), 4L)


  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)

  expect_is(myEns2, "caretEnsemble")
  expect_is(myEns1, "caretEnsemble")
})

test_that("Users can pass a custom tuneList", {
  skip_on_cran()
  # User specifies methods and tuning parameters specifically using a tuneList
  myControl <- trainControl(
    method = "cv",
    number = 3L,
    p = 0.75,
    savePredictions = "final",
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    returnResamp = "final",
    returnData = TRUE,
    verboseIter = FALSE
  )

  tuneTest <- list(
    rpart = caretModelSpec(
      method = "rpart",
      tuneGrid = data.frame(.cp = c(0.01, 0.001, 0.1, 1.0))
    ),
    knn = caretModelSpec(
      method = "knn",
      tuneLength = 9L
    ),
    svmRadial = caretModelSpec(
      method = "svmRadial",
      tuneLength = 3L
    )
  )

  test2a <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    trControl = myControl,
    tuneList = tuneTest
  )

  myEns2a <- caretEnsemble(test2a)
  expect_is(myEns2a, "caretEnsemble")
  expect_is(test2a, "caretList")
  expect_equal(nrow(test2a[[1L]]$results), 4L)
  expect_equal(nrow(test2a[[2L]]$results), 9L)
  expect_equal(nrow(test2a[[3L]]$results), 3L)
})

context("User tuneTest parameters are respected and model is ensembled")
test_that("User tuneTest parameters are respected and model is ensembled", {
  skip_on_cran()
  myControl <- trainControl(
    method = "cv",
    number = 3L,
    p = 0.75,
    savePredictions = "final",
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    returnResamp = "final",
    returnData = TRUE,
    verboseIter = FALSE
  )
  tuneTest <- list(
    nnet = caretModelSpec(
      method = "nnet",
      tuneLength = 3L,
      trace = FALSE,
      softmax = FALSE
    )
  )
  test <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    trControl = myControl,
    tuneList = tuneTest
  )
  ens <- caretEnsemble(test)
  expect_is(ens, "caretEnsemble")
  expect_is(test, "caretList")
  expect_equal(nrow(test[[1L]]$results), 3L * 3L)
  expect_false(test[[1L]]$finalModel$softmax)
})

context("Formula interface for caretList works")
test_that("User tuneTest parameters are respected and model is ensembled", {
  tuneTest <- list(
    rpart = list(method = "rpart", tuneLength = 2L),
    nnet = list(method = "nnet", tuneLength = 2L, trace = FALSE),
    glm = list(method = "glm")
  )
  x <- iris[, 1L:3L]
  y <- iris[, 4L]
  set.seed(42L)
  expect_warning(
    test_default <- caretList(
      x = x,
      y = y,
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
  set.seed(42L)
  expect_warning(
    test_flma <- caretList(
      y ~ .,
      data = data.frame(y = y, x),
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
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

test_that("Regression Models", {
  myControl2 <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePrediction = TRUE,
    returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE
  )

  test1 <- caretList(
    x = train[, c(-23L, -1L)],
    y = train[, 1L],
    trControl = myControl2,
    methodList = c("glm", "lm")
  )
  test2 <- caretList(
    x = train[, c(-23L, -1L)],
    y = train[, 1L],
    trControl = myControl2,
    methodList = c("glm", "ppr", "lm")
  )

  ens1 <- caretEnsemble(test1)
  ens2 <- caretEnsemble(test2)

  expect_is(test1, "caretList")
  expect_is(test2, "caretList")

  expect_is(ens1, "caretEnsemble")
  expect_is(ens2, "caretEnsemble")
})

test_that("methodCheck stops for invalid method type", {
  expect_error(methodCheck(list(123L)), "Method \"123\" is invalid.")
  expect_error(methodCheck(list("invalid_method")), "The following models are not valid caret models: invalid_method")
})

test_that("is.caretList correctly identifies caretList objects", {
  expect_true(is.caretList(structure(list(), class = "caretList")))
  expect_false(is.caretList(list()))
})

test_that("as.caretList stops for null object", {
  expect_error(as.caretList(NULL), "object is null")
})

test_that("as.caretList.list stops for non-list object", {
  expect_error(as.caretList.list(1L), "object must be a list of caret models")
})

test_that("predict.caretList doesn't care about missing training data", {
  new_model_list <- lapply(models.class, function(x) {
    x$trainingData <- NULL
    x
  })
  new_model_list <- as.caretList(new_model_list)
  pred <- predict.caretList(new_model_list)
  expect_is(pred, "data.table")
  expect_equal(nrow(pred), 150L)
  expect_named(pred, names(new_model_list))
})

test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = list(method = "custom_method")), class = "train")
  expect_equal(extractModelName(mock_model), "custom_method")
})

test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = "custom_method", class = "train"))
  expect_equal(extractModelName(mock_model), "custom_method")
})

test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = "custom", class = "train", modelInfo = list(method = "custom_method")))
  expect_equal(extractModelName(mock_model), "custom_method")
})

test_that("as.caretList.list fails on NULL object", {
  err <- "object requires all elements of list to be caret models"
  expect_error(as.caretList(list(NULL)), err)
})

test_that("predict.caretList works when the progress bar is turned off", {
  set.seed(42L)
  N <- 100L
  noise_level <- 1L / 10L
  X <- data.frame(
    a = runif(N),
    b = runif(N)
  )
  y <- 7.5 - 10.0 * X$a + 5.0 * X$b + noise_level * rnorm(N)
  models <- caretList(
    X, y,
    tuneLength = 1L,
    methodList = "lm",
    trControl = trainControl(
      method = "cv", number = 2L,
      savePredictions = "final",
      index = createFolds(y, k = 2L)
    )
  )
  pred <- predict(models, X, verbose = FALSE)[["lm"]]
  rmse <- sqrt(mean((y - pred)^2L))
  expect_lt(rmse, noise_level)
})

test_that("caretList handles missing data correctly", {
  data(iris)
  iris_with_na <- iris
  x <- iris_with_na[, 1L:4L]
  y <- iris_with_na[, 5L]
  x[sample.int(nrow(x), 10L), sample.int(ncol(x), 2L)] <- NA

  myControl <- trainControl(
    method = "cv", number = 3L,
    p = 0.75, savePrediction = "final",
    returnResamp = "final",
    returnData = TRUE, verboseIter = FALSE,
    classProbs = TRUE
  )

  models <- caretList(
    x = x,
    y = y,
    methodList = "rpart",
    trControl = myControl
  )

  expect_s3_class(models, "caretList")
  expect_length(models, 1L)
})

test_that("caretList handles new factor levels in prediction", {
  data(iris)
  idx <- seq_len(nrow(iris))
  idx_train <- sample(idx, 100L)
  idx_test <- setdiff(idx, idx_train)
  train_data <- iris[idx_train, ]
  test_data <- iris[idx_test, ]
  test_data$Species <- factor(as.character(test_data$Species), levels = c(levels(iris$Species), "NewSpecies"))
  test_data$Species[1L] <- "NewSpecies"

  models <- caretList(
    x = train_data[, 1L:4L],
    y = train_data[, 5L],
    methodList = c("rpart", "rf"),
    trControl = trainControl(method = "cv", number = 2L, allowParallel = FALSE, classProbs = TRUE)
  )

  pred <- predict(models, newdata = test_data)
  expect_is(pred, "data.table")
  expect_equal(nrow(pred), nrow(test_data))
})

test_that("caretList handles large number of predictors", {
  set.seed(123L)
  n <- 100L
  p <- 1000L
  X <- data.frame(matrix(rnorm(n * p), n, p))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))

  models <- caretList(
    x = X,
    y = y,
    methodList = c("glmnet", "rpart"),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      allowParallel = FALSE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
  )


  expect_s3_class(models, "caretList")
  expect_length(models, 2L)
})

test_that("caretList handles imbalanced data", {
  set.seed(123L)
  n <- 1000L
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- factor(c(rep("A", 950L), rep("B", 50L)))

  models <- caretList(
    x = X,
    y = y,
    methodList = c("glmnet", "rpart"),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      sampling = "down",
      allowParallel = FALSE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
  )

  expect_s3_class(models, "caretList")
  expect_length(models, 2L)
})

test_that("caretList handles custom performance metrics", {
  data(iris)
  custom_summary <- function(data, lev = NULL, model = NULL) {
    c(default = mean(data$obs == data$pred))
  }

  models <- caretList(
    x = iris[, 1L:4L],
    y = iris[, 5L],
    metric = "default",
    methodList = c("rpart", "rf"),
    trControl = trainControl(
      method = "cv",
      number = 2L,
      summaryFunction = custom_summary,
      allowParallel = FALSE,
      classProbs = TRUE
    )
  )

  expect_s3_class(models, "caretList")
  expect_true(all(sapply(models, function(m) "default" %in% colnames(m$results))))
})
