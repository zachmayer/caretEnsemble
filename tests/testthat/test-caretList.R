# Test caretList
set.seed(442L)

train <- caret::twoClassSim(
  n = 1000L, intercept = -8L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)
test <- caret::twoClassSim(
  n = 1500L, intercept = -7L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)

###############################################
testthat::context("Ancillary caretList functions and errors")
################################################
testthat::test_that("caretModelSpec returns valid specs", {
  tuneList <- list(
    rf1 = caretModelSpec(),
    rf2 = caretModelSpec(method = "rf", tuneLength = 5L),
    caretModelSpec(method = "rpart"),
    caretModelSpec(method = "knn", tuneLength = 10L)
  )
  tuneList <- caretEnsemble::tuneCheck(tuneList)
  testthat::expect_type(tuneList, "list")
  testthat::expect_length(tuneList, 4L)
  testthat::expect_equal(sum(duplicated(names(tuneList))), 0L)
})

testthat::test_that("caretModelSpec and checking functions work as expected", {
  all_models <- sort(unique(modelLookup()$model))
  for (model in all_models) {
    testthat::expect_equal(caretModelSpec(model, tuneLength = 5L, preProcess = "knnImpute")$method, model)
  }

  tuneList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  all_models_check <- tuneCheck(tuneList)
  testthat::expect_is(all_models_check, "list")
  testthat::expect_length(all_models, length(all_models_check))

  tuneList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  names(tuneList) <- all_models
  names(tuneList)[c(1L, 5L, 10L)] <- ""
  all_models_check <- tuneCheck(tuneList)
  testthat::expect_is(all_models_check, "list")
  testthat::expect_length(all_models, length(all_models_check))

  methodCheck(all_models)
  err <- "The following models are not valid caret models: THIS_IS_NOT_A_REAL_MODEL"
  testthat::expect_error(methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL")), err)
  testthat::expect_error(methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL", "GBM")))
})

testthat::test_that("Target extraction functions work", {
  data(iris)
  testthat::expect_equal(extractCaretTarget(iris[, 1L:4L], iris[, 5L]), iris[, 5L])
  testthat::expect_equal(extractCaretTarget(iris[, 2L:5L], iris[, 1L]), iris[, 1L])
  testthat::expect_equal(extractCaretTarget(Species ~ ., iris), iris[, "Species"])
  testthat::expect_equal(extractCaretTarget(Sepal.Width ~ ., iris), iris[, "Sepal.Width"])
})

testthat::test_that("caretList errors for bad models", {
  data(iris)

  # Basic checks
  testthat::expect_error(caretList(Sepal.Width ~ ., iris), "Please either define a methodList or tuneList")
  testthat::expect_warning(
    caretList(Sepal.Width ~ ., iris, methodList = c("lm", "lm")),
    "Duplicate entries in methodList. Using unqiue methodList values."
  )
  testthat::expect_is(caretList(Sepal.Width ~ ., iris, methodList = "lm", continue_on_fail = TRUE), "caretList")

  # Check that by default a bad model kills the training job
  bad <- list(
    bad = caretModelSpec(method = "glm", tuneLength = 1L)
  )
  testthat::expect_output(
    testthat::expect_warning(
      testthat::expect_error(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = bad),
        regexp = "Stopping" # Stop training on the first error.  This is the mssage straight from train.
      ),
      regexp = "model fit failed for Fold1"
    ),
    regexp = "Something is wrong; all the Accuracy metric values are missing:"
  )
  testthat::expect_output(
    testthat::expect_warning(
      testthat::expect_error(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = bad, continue_on_fail = TRUE),
        regexp = "caret:train failed for all models. Please inspect your data."
      ),
      regexp = "model fit failed for Fold1"
    ),
    regexp = "Something is wrong; all the Accuracy metric values are missing:"
  )

  # Check that at least one good model + continue_on_fail works
  good_bad <- list(
    good = caretModelSpec(method = "glmnet", tuneLength = 1L),
    bad = caretModelSpec(method = "glm", tuneLength = 1L)
  )
  testthat::expect_s3_class(
    testthat::expect_output(
      testthat::expect_warning(
        caretList(iris[, 1L:4L], iris[, 5L], tuneList = good_bad, continue_on_fail = TRUE),
        regexp = "model fit failed for Fold1"
      ),
      regexp = "Something is wrong; all the Accuracy metric values are missing:"
    ), "caretList"
  )
})

testthat::test_that("caretList predictions", {
  testthat::expect_warning(
    models <- caretList(
      iris[, 1L:2L], iris[, 3L],
      tuneLength = 1L, verbose = FALSE,
      methodList = "rf", tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE))
    ), "There were missing values in resampled performance measures."
  )

  p1 <- predict(models)
  p2 <- predict(models, newdata = iris[100L, 1L:2L])
  p3 <- predict(models, newdata = iris[110L, 1L:2L])
  testthat::expect_is(p1, "data.table")
  testthat::expect_is(p1[[1L]], "numeric")
  testthat::expect_is(p1[[2L]], "numeric")
  testthat::expect_named(models, colnames(p1))
  testthat::expect_is(p2, "data.table")
  testthat::expect_is(p2[[1L]], "numeric")
  testthat::expect_is(p2[[2L]], "numeric")
  testthat::expect_named(models, colnames(p2))
  testthat::expect_is(p3, "data.table")
  testthat::expect_is(p3[[1L]], "numeric")
  testthat::expect_is(p3[[2L]], "numeric")
  testthat::expect_named(models, colnames(p3))

  models <- caretList(
    iris[, 1L:2L], iris[, 5L],
    tuneLength = 1L, verbose = FALSE,
    methodList = "rf",
    tuneList = list(nnet = caretModelSpec(method = "nnet", trace = FALSE))
  )

  p2 <- predict(models, excluded_class_id = 0L)
  p3 <- predict(models, newdata = iris[, 1L:2L], excluded_class_id = 0L)
  testthat::expect_is(p2, "data.table")
  testthat::expect_is(p2[[1L]], "numeric")
  testthat::expect_is(p2[[2L]], "numeric")
  testthat::expect_is(p2[[3L]], "numeric")
  testthat::expect_is(p2[[4L]], "numeric")
  testthat::expect_is(p3, "data.table")
  testthat::expect_is(p3[[1L]], "numeric")
  testthat::expect_is(p3[[2L]], "numeric")
  testthat::expect_is(p3[[3L]], "numeric")
  testthat::expect_is(p3[[4L]], "numeric")
  testthat::expect_equal(
    length(names(models)) * nlevels(as.factor(iris[, 5L])),
    length(colnames(p3))
  ) # check that we have the right number of columns
  testthat::expect_identical(dim(p2), dim(p3))
  testthat::expect_named(p2, names(p3))

  modelnames <- names(models)
  classes <- levels(iris[, 5L])
  combinations <- expand.grid(classes, modelnames)
  correct_colnames <- apply(combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  testthat::expect_equal(
    correct_colnames,
    colnames(p3)
  ) # check the column names are correct and ordered correctly (methodname_classname)

  # Check that bad model types error
  # This error message is weird though
  models[[1L]]$modelType <- "Bogus"
  testthat::expect_error(predict(models), "mean is not meaningful for factors.")
})

testthat::test_that("as.caretList.list returns a caretList object", {
  modelList <- caretList(Sepal.Length ~ Sepal.Width,
    head(iris, 50L),
    methodList = c("glm", "lm", "knn")
  )
  class(modelList) <- "list"
  testthat::expect_is(as.caretList(modelList), "caretList")
})

#############################################################
testthat::context("Bad characters in target variable names and model names")
#############################################################
testthat::test_that("Target variable names with character | are not allowed", {
  bad_iris <- iris[1L:100L, ]
  bad_iris[, 5L] <- gsub("versicolor", "versicolor|1", bad_iris[, 5L], fixed = TRUE)
  bad_iris[, 5L] <- gsub("setosa", "setosa|2", bad_iris[, 5L], fixed = TRUE)
  bad_iris[, 5L] <- as.factor(as.character(bad_iris[, 5L]))

  # Expect an error from caret
  testthat::expect_error(
    caretList(
      x = bad_iris[, -5L],
      y = bad_iris[, 5L],
      methodList = c("rpart", "glmnet")
    ), "At least one of the class levels is not a valid R variable name; This will cause errors when class prob"
  )
})

testthat::test_that("Character | in model names is transformed into a point", {
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
    )
  )
  testthat::expect_named(model_list, c("nnet.1", "nnet.2"))
})

###############################################
testthat::context("We can fit models with a mix of methodList and tuneList")
################################################
testthat::test_that("We can fit models with a mix of methodList and tuneList", {
  myList <- list(
    rpart = caretModelSpec(method = "rpart", tuneLength = 10L),
    rf = caretModelSpec(method = "rf", tuneGrid = data.table::data.table(mtry = 2L))
  )
  testthat::expect_warning(
    test <- caretList(
      x = iris[, 1L:3L],
      y = iris[, 4L],
      methodList = c("knn", "glm"),
      tuneList = myList
    ), "There were missing values in resampled performance measures."
  )
  testthat::expect_is(test, "caretList")
  testthat::expect_is(caretEnsemble(test), "caretEnsemble")
  testthat::expect_length(test, 4L)
  methods <- sapply(test, function(x) x$method)
  names(methods) <- NULL
  testthat::expect_equal(methods, c("rpart", "rf", "knn", "glm"))
})

################################################
testthat::context("We can handle different CV methods")
################################################
testthat::test_that("We can handle different CV methods", {
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

    testthat::expect_warning(
      models <- caretList(
        x = x,
        y = y,
        tuneLength = 2L,
        methodList = c("rpart", "rf")
      ), "There were missing values in resampled performance measures."
    )
    ens <- caretStack(models, method = "glm")

    invisible(sapply(models, testthat::expect_is, class = "train"))

    ens <- caretEnsemble(models)

    testthat::expect_is(ens, "caretEnsemble")

    ens <- caretStack(models, method = "glm")

    testthat::expect_is(ens, "caretStack")
  }
})

testthat::test_that("Non standard cv methods work", {
  data(iris)
  models <- lapply(
    c("boot632", "LOOCV", "none"),
    function(m) {
      model <- caret::train(
        x = iris[, 1L:2L],
        y = iris[, 3L],
        tuneLength = 1L,
        data = iris,
        method = "rf",
        trControl = caret::trainControl(
          method = m,
          savePredictions = "final"
        )
      )
      testthat::expect_is(model, "train")
      model
    }
  )
  caret_list <- as.caretList(models)
  p <- predict(caret_list, newdata = iris[, 1L:2L])
  testthat::expect_s3_class(p, "data.table")
})

###############################################
testthat::context("Classification models")
################################################
testthat::test_that("Classification models", {
  # Simple two method list
  # Warning because we Are going to auto-set indexes
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    methodList = c("knn", "glm")
  )

  testthat::expect_is(test1, "caretList")
  testthat::expect_is(caretEnsemble(test1), "caretEnsemble")
  testthat::expect_is(caretEnsemble(test1), "caretEnsemble")
})

testthat::test_that("Longer tests for Classification models", {
  # Simple two method list
  # Warning because we Are going to auto-set indexes
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    methodList = c("knn", "glm")
  )

  testthat::expect_is(test1, "caretList")
  testthat::expect_is(caretEnsemble(test1), "caretEnsemble")
  testthat::expect_is(caretEnsemble(test1), "caretEnsemble")

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    methodList = c("knn", "glm", "rpart")
  )

  test3 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    metric = "ROC",
    methodList = c("svmLinear", "knn", "glm")
  )

  testthat::expect_is(test2, "caretList")
  testthat::expect_is(test3, "caretList")
  testthat::expect_is(caretEnsemble(test2), "caretEnsemble")
  testthat::expect_is(caretEnsemble(test3), "caretEnsemble")

  testthat::expect_identical(test2[[1L]]$metric, "ROC")
  testthat::expect_identical(test3[[1L]]$metric, "ROC")
})

testthat::test_that("Test that caretList preserves user specified error functions", {
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 7L,
    methodList = c("knn", "rpart", "glm")
  )

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 4L,
    methodList = c("knn", "rpart", "glm")
  )

  testthat::expect_identical(test1[[1L]]$metric, "ROC")
  testthat::expect_identical(test2[[1L]]$metric, "ROC")

  testthat::expect_equal(nrow(test1[[1L]]$results), 7L)
  testthat::expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  testthat::expect_equal(nrow(test2[[1L]]$results), 4L)

  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)
  testthat::expect_is(myEns2, "caretEnsemble")
  testthat::expect_is(myEns1, "caretEnsemble")

  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 7L,
    methodList = c("knn", "rpart", "glm")
  )

  test2 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    tuneLength = 4L,
    methodList = c("knn", "rpart", "glm")
  )

  testthat::expect_identical(test1[[1L]]$metric, "ROC")
  testthat::expect_identical(test2[[1L]]$metric, "ROC")

  testthat::expect_equal(nrow(test1[[1L]]$results), 7L)
  testthat::expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  testthat::expect_equal(nrow(test2[[1L]]$results), 4L)


  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)

  testthat::expect_is(myEns2, "caretEnsemble")
  testthat::expect_is(myEns1, "caretEnsemble")
})

testthat::test_that("Users can pass a custom tuneList", {
  tuneTest <- list(
    rpart = caretModelSpec(
      method = "rpart",
      tuneGrid = data.table::data.table(.cp = c(0.01, 0.001, 0.1, 1.0))
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
    tuneList = tuneTest
  )

  myEns2a <- caretEnsemble(test2a)
  testthat::expect_is(myEns2a, "caretEnsemble")
  testthat::expect_is(test2a, "caretList")
  testthat::expect_equal(nrow(test2a[[1L]]$results), 4L)
  testthat::expect_equal(nrow(test2a[[2L]]$results), 9L)
  testthat::expect_equal(nrow(test2a[[3L]]$results), 3L)
})

testthat::context("User tuneTest parameters are respected and model is ensembled")
testthat::test_that("User tuneTest parameters are respected and model is ensembled", {
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
    tuneList = tuneTest
  )
  ens <- caretEnsemble(test)
  testthat::expect_is(ens, "caretEnsemble")
  testthat::expect_is(test, "caretList")
  testthat::expect_equal(nrow(test[[1L]]$results), 3L * 3L)
  testthat::expect_false(test[[1L]]$finalModel$softmax)
})

testthat::context("Formula interface for caretList works")
testthat::test_that("User tuneTest parameters are respected and model is ensembled", {
  tuneTest <- list(
    rpart = list(method = "rpart", tuneLength = 2L),
    nnet = list(method = "nnet", tuneLength = 2L, trace = FALSE),
    glm = list(method = "glm")
  )
  x <- iris[, 1L:3L]
  y <- iris[, 4L]
  set.seed(42L)
  testthat::expect_warning(
    test_default <- caretList(
      x = x,
      y = y,
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
  set.seed(42L)
  testthat::expect_warning(
    test_flma <- caretList(
      y ~ .,
      data = data.table::data.table(y = y, x),
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
  ens_default <- caretEnsemble(test_default)
  ens_flma <- caretEnsemble(test_flma)
  testthat::expect_is(ens_default, "caretEnsemble")
  testthat::expect_is(ens_flma, "caretEnsemble")

  testthat::expect_equal(ens_default$RMSE, ens_flma$RMSE)
  testthat::expect_equal(ens_default$weights, ens_flma$weights)
})

###############################################
testthat::context("Regression models")
###############################################

testthat::test_that("Regression Models", {
  test1 <- caretList(
    x = train[, c(-23L, -1L)],
    y = train[, 1L],
    methodList = c("glm", "lm")
  )
  test2 <- caretList(
    x = train[, c(-23L, -1L)],
    y = train[, 1L],
    methodList = c("glm", "ppr", "lm")
  )

  ens1 <- caretEnsemble(test1)
  ens2 <- caretEnsemble(test2)

  testthat::expect_is(test1, "caretList")
  testthat::expect_is(test2, "caretList")

  testthat::expect_is(ens1, "caretEnsemble")
  testthat::expect_is(ens2, "caretEnsemble")
})

testthat::test_that("methodCheck stops for invalid method type", {
  testthat::expect_error(methodCheck(list(123L)), "Method \"123\" is invalid.")
  testthat::expect_error(methodCheck(list("invalid_method")), "The following models are not valid caret models: invalid_method")
})

testthat::test_that("is.caretList correctly identifies caretList objects", {
  testthat::expect_true(is.caretList(structure(list(), class = "caretList")))
  testthat::expect_false(is.caretList(list()))
})

testthat::test_that("as.caretList stops for null object", {
  testthat::expect_error(as.caretList(NULL), "object is null")
})

testthat::test_that("as.caretList.list stops for non-list object", {
  testthat::expect_error(as.caretList.list(1L), "object must be a list of caret models")
})

testthat::test_that("predict.caretList doesn't care about missing training data", {
  new_model_list <- lapply(models.class, function(x) {
    x$trainingData <- NULL
    x
  })
  new_model_list <- as.caretList(new_model_list)
  pred <- predict.caretList(new_model_list)
  testthat::expect_is(pred, "data.table")
  testthat::expect_equal(nrow(pred), 150L)
  testthat::expect_named(pred, names(new_model_list))
})

testthat::test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = list(method = "custom_method")), class = "train")
  testthat::expect_equal(extractModelName(mock_model), "custom_method")
})

testthat::test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = "custom_method", class = "train"))
  testthat::expect_equal(extractModelName(mock_model), "custom_method")
})

testthat::test_that("extractModelName handles custom models correctly", {
  mock_model <- structure(list(method = "custom", class = "train", modelInfo = list(method = "custom_method")))
  testthat::expect_equal(extractModelName(mock_model), "custom_method")
})

testthat::test_that("as.caretList.list fails on NULL object", {
  err <- "object requires all elements of list to be caret models"
  testthat::expect_error(as.caretList(list(NULL)), err)
})

testthat::test_that("predict.caretList works when the progress bar is turned off", {
  set.seed(42L)
  N <- 100L
  noise_level <- 1L / 10L
  X <- data.table::data.table(
    a = runif(N),
    b = runif(N)
  )
  y <- 7.5 - 10.0 * X$a + 5.0 * X$b + noise_level * rnorm(N)
  models <- caretList(
    X, y,
    tuneLength = 1L,
    methodList = "lm"
  )
  pred <- predict(models, X, verbose = FALSE)[["lm"]]
  rmse <- sqrt(mean((y - pred)^2L))
  testthat::expect_lt(rmse, noise_level)
})

testthat::test_that("caretList handles missing data correctly", {
  data(iris)
  iris_with_na <- iris
  x <- iris_with_na[, 1L:4L]
  y <- iris_with_na[, 5L]
  x[sample.int(nrow(x), 10L), sample.int(ncol(x), 2L)] <- NA

  models <- caretList(
    x = x,
    y = y,
    methodList = "rpart"
  )

  testthat::expect_s3_class(models, "caretList")
  testthat::expect_length(models, 1L)
})

testthat::test_that("caretList handles new factor levels in prediction", {
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
    methodList = c("rpart", "rf")
  )

  pred <- predict(models, newdata = test_data)
  testthat::expect_is(pred, "data.table")
  testthat::expect_equal(nrow(pred), nrow(test_data))
})

testthat::test_that("caretList handles large number of predictors", {
  set.seed(123L)
  n <- 100L
  p <- 1000L
  X <- data.table::data.table(matrix(rnorm(n * p), n, p))
  y <- factor(sample(c("A", "B"), n, replace = TRUE))

  models <- caretList(
    x = X,
    y = y,
    methodList = c("glmnet", "rpart")
  )

  testthat::expect_s3_class(models, "caretList")
  testthat::expect_length(models, 2L)
})

testthat::test_that("caretList handles imbalanced data", {
  set.seed(123L)
  n <- 1000L
  X <- data.table::data.table(x1 = rnorm(n), x2 = rnorm(n))
  y <- factor(c(rep("A", 950L), rep("B", 50L)))

  models <- caretList(
    x = X,
    y = y,
    methodList = c("glmnet", "rpart")
  )

  testthat::expect_s3_class(models, "caretList")
  testthat::expect_length(models, 2L)
})


testthat::test_that("caretList handles custom performance metrics", {
  data(iris)
  models <- caretList(
    x = iris[, 1L:4L],
    y = iris[, 5L],
    metric = "default",
    methodList = c("rpart", "rf"),
    trControl = caret::trainControl(
      method = "cv",
      number = 2L,
      summaryFunction = function(data, lev = NULL, model = NULL) c(default = mean(data$obs == data$pred)),
      allowParallel = FALSE,
      classProbs = TRUE
    )
  )
  testthat::expect_s3_class(models, "caretList")
  testthat::expect_true(all(sapply(models, function(m) "default" %in% colnames(m$results))))
})
