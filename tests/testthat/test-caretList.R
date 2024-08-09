# Test caretList
set.seed(442L)

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

train <- caret::twoClassSim(
  n = 1000L, intercept = -8L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)
test <- caret::twoClassSim(
  n = 1500L, intercept = -7L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)

###############################################
testthat::context("caretModelSpec, tuneCheck, methodCheck")
###############################################
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
  testthat::expect_identical(sum(duplicated(names(tuneList))), 0L)
})

testthat::test_that("caretModelSpec and checking functions work as expected", {
  all_models <- sort(unique(caret::modelLookup()$model))
  for (model in all_models) {
    testthat::expect_identical(caretModelSpec(model, tuneLength = 5L, preProcess = "knnImpute")$method, model)
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

testthat::test_that("methodCheck stops for invalid method type", {
  testthat::expect_error(methodCheck(list(123L)), "Method \"123\" is invalid.")
  testthat::expect_error(
    methodCheck(list("invalid_method")),
    "The following models are not valid caret models: invalid_method"
  )
})

###############################################
testthat::context("extractCaretTarget")
###############################################

testthat::test_that("Target extraction functions work", {
  data(iris)
  testthat::expect_identical(extractCaretTarget(iris[, 1L:4L], iris[, 5L]), iris[, 5L])
  testthat::expect_identical(extractCaretTarget(iris[, 2L:5L], iris[, 1L]), iris[, 1L])
  testthat::expect_identical(extractCaretTarget(Species ~ ., iris), iris[, "Species"])
  testthat::expect_identical(extractCaretTarget(Sepal.Width ~ ., iris), iris[, "Sepal.Width"])
})

###############################################
testthat::context("S3 methods for caretList")
################################################

testthat::test_that("[.caretList subsets caretList objects correctly", {
  subset_models <- models.class[1L:2L]

  testthat::expect_s3_class(subset_models, "caretList")
  testthat::expect_length(subset_models, 2L)
  testthat::expect_true(all(names(subset_models) %in% names(models.class)[1L:2L]))
})

testthat::test_that("c.caretList combines caretList objects correctly", {
  models_class1 <- models.class[1L:2L]
  models_class2 <- models.class[3L:4L]
  class(models_class1) <- class(models_class2) <- "caretList"

  combined_models <- c(models_class1, models_class2)

  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, length(models.class))
  testthat::expect_true(all(names(combined_models) %in% names(models.class)))
})

testthat::test_that("c.caretList combines caretList and train objects correctly", {
  models_class1 <- models.class[1L:2L]
  class(models_class1) <- "caretList"
  single_model <- models.class[[3L]]

  combined_models <- c(models_class1, single_model)

  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, 3L)
  testthat::expect_true(all(names(combined_models) %in% names(models.class)))
})

testthat::test_that("c.caretList handles duplicate names", {
  models_class1 <- models.class[1L:2L]
  class(models_class1) <- "caretList"

  combined_models <- c(models_class1, models_class1)

  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, 4L)
  testthat::expect_true(all(make.names(rep(names(models_class1), 2L), unique = TRUE) %in% names(combined_models)))
})

testthat::test_that("c.caretList and c.train fail for invalid inputs", {
  testthat::expect_error(c.caretList(list(a = 1L, b = 2L)), "class of modelList1 must be 'caretList' or 'train'")
  testthat::expect_error(c.train(list(a = 1L, b = 2L)), "class of modelList1 must be 'caretList' or 'train'")
})

testthat::test_that("c can bind two caretList objects", {
  bigList <- c(models.class, models.reg)
  testthat::expect_is(bigList, "caretList")
  testthat::expect_identical(anyDuplicated(names(bigList)), 0L)
  testthat::expect_length(unique(names(bigList)), 8L)
})

testthat::test_that("c.caretList can bind a caretList and train object", {
  bigList <- c(models.class, models.reg[[1L]])
  testthat::expect_is(bigList, "caretList")
  testthat::expect_identical(anyDuplicated(names(bigList)), 0L)
  testthat::expect_length(unique(names(bigList)), 5L)
})

testthat::test_that("c.caretList stops for invalid class", {
  testthat::expect_error(c.caretList(list()), "class of modelList1 must be 'caretList' or 'train'")
})

testthat::test_that("as.caretList.list converts list to caretList", {
  model_list <- list(model1 = models.class[[1L]], model2 = models.class[[2L]])
  caretlist_object <- as.caretList(model_list)

  testthat::expect_s3_class(caretlist_object, "caretList")
  testthat::expect_length(caretlist_object, 2L)
  testthat::expect_true(all(names(caretlist_object) %in% names(model_list)))
})

testthat::test_that("as.caretList.list fails for invalid inputs", {
  testthat::expect_error(as.caretList(list(a = 1L, b = 2L)), "object requires all elements of list to be caret models")
})

testthat::test_that("as.caretList.list names lists without names", {
  models.no.name <- models.class
  names(models.no.name) <- NULL
  class(models.no.name) <- "list"
  testthat::expect_null(names(models.no.name))
  cl <- as.caretList(models.no.name)
  testthat::expect_named(cl, unname(vapply(models.class, "[[", character(1L), "method")))
})

testthat::test_that("as.caretList fails on non-list", {
  testthat::expect_error(as.caretList(1L), "object must be a list")
})

testthat::test_that("as.caretList stops for null object", {
  testthat::expect_error(as.caretList(NULL), "object is null")
})

testthat::test_that("as.caretList.list stops for non-list object", {
  testthat::expect_error(as.caretList.list(1L), "object must be a list of caret models")
})

testthat::test_that("as.caretList.list fails on NULL object", {
  err <- "object requires all elements of list to be caret models"
  testthat::expect_error(as.caretList(list(NULL)), err)
})

###############################################
testthat::context("predict.caretList")
###############################################
testthat::test_that("predict.caretList works for classification and regression", {
  class_preds <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  reg_preds <- predict(models.reg, newdata = X.reg)

  testthat::expect_is(class_preds, "data.table")
  testthat::expect_is(reg_preds, "data.table")
  testthat::expect_identical(nrow(class_preds), nrow(X.class))
  testthat::expect_identical(nrow(reg_preds), nrow(X.reg))
  testthat::expect_identical(ncol(class_preds), length(models.class) * 2L)
  testthat::expect_identical(ncol(reg_preds), length(models.reg))
})

testthat::test_that("predict.caretList handles type='prob' for classification", {
  class_probs <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  testthat::expect_is(class_probs, "data.table")
  testthat::expect_identical(nrow(class_probs), nrow(X.class))
  testthat::expect_identical(ncol(class_probs), length(models.class) * nlevels(Y.class))
})

testthat::test_that("Stacked predictions for caret lists works", {
  best_preds_class <- predict(models.class)
  best_preds_reg <- predict(models.reg)

  testthat::expect_is(best_preds_class, "data.table")
  testthat::expect_is(best_preds_reg, "data.table")

  testthat::expect_named(best_preds_class, names(models.class))
  testthat::expect_named(best_preds_reg, names(models.reg))
})

testthat::test_that("Stacked predictions works with different resampling strategies", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$Resample <- "WEIRD_SAMPLING"
  testthat::expect_is(predict(models.class.inconsistent), "data.table")
})

testthat::test_that("Stacked predictions works if the row indexes differ", {
  models.class.inconsistent <- models.class
  models.class.inconsistent[[1L]]$pred$rowIndex <- rev(models.class.inconsistent[[1L]]$pred$rowIndex)
  big_preds <- rbind(models.class.inconsistent[[2L]]$pred, models.class.inconsistent[[2L]]$pred)
  models.class.inconsistent[[2L]]$pred <- big_preds
  testthat::expect_is(predict(models.class.inconsistent), "data.table")
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


testthat::test_that("predict.caretList doesn't care about missing training data", {
  new_model_list <- lapply(models.class, function(x) {
    x$trainingData <- NULL
    x
  })
  new_model_list <- as.caretList(new_model_list)
  pred <- predict.caretList(new_model_list)
  testthat::expect_is(pred, "data.table")
  testthat::expect_identical(nrow(pred), 150L)
  testthat::expect_named(pred, names(new_model_list))
})

###############################################
testthat::context("caretList")
###############################################

testthat::test_that("caretList errors for bad models", {
  data(iris)

  # Basic checks
  testthat::expect_error(caretList(Sepal.Width ~ ., iris), "Please either define a methodList or tuneList")
  testthat::expect_warning(
    caretList(Sepal.Width ~ ., iris, methodList = c("lm", "lm")),
    "Duplicate entries in methodList. Using unique methodList values."
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
        regexp = "Stopping" # Stop training on the first error. This is the mssage straight from train.
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
  models <- testthat::expect_warning(
    caretList(
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
  testthat::expect_identical(
    length(names(models)) * nlevels(as.factor(iris[, 5L])),
    length(colnames(p3))
  ) # check that we have the right number of columns
  testthat::expect_identical(dim(p2), dim(p3))
  testthat::expect_named(p2, names(p3))

  modelnames <- names(models)
  classes <- levels(iris[, 5L])
  combinations <- expand.grid(classes, modelnames)
  correct_colnames <- apply(combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  testthat::expect_named(
    p3,
    correct_colnames
  ) # check the column names are correct and ordered correctly (methodname_classname)
})

testthat::test_that("as.caretList.list returns a caretList object", {
  modelList <- caretList(Sepal.Length ~ Sepal.Width,
    head(iris, 50L),
    methodList = c("glm", "lm", "knn")
  )
  class(modelList) <- "list"
  testthat::expect_is(as.caretList(modelList), "caretList")
})

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

testthat::test_that("We can fit models with a mix of methodList and tuneList", {
  myList <- list(
    rpart = caretModelSpec(method = "rpart", tuneLength = 10L),
    rf = caretModelSpec(method = "rf", tuneGrid = data.table::data.table(mtry = 2L))
  )
  test <- testthat::expect_warning(
    caretList(
      x = iris[, 1L:3L],
      y = iris[, 4L],
      methodList = c("knn", "glm"),
      tuneList = myList
    ), "There were missing values in resampled performance measures."
  )
  testthat::expect_is(test, "caretList")
  testthat::expect_is(caretEnsemble(test), "caretEnsemble")
  testthat::expect_length(test, 4L)
  methods <- vapply(test, function(x) x$method, character(1L))
  names(methods) <- NULL
  testthat::expect_identical(methods, c("rpart", "rf", "knn", "glm"))
})

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

    models <- testthat::expect_warning(
      caretList(
        x = x,
        y = y,
        tuneLength = 2L,
        methodList = c("rpart", "rf")
      ), "There were missing values in resampled performance measures."
    )
    ens <- caretStack(models, method = "glm")

    for (x in models) {
      testthat::expect_s3_class(x, "train")
    }

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
    methodList = c("rpart", "knn", "glm")
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

  testthat::expect_identical(nrow(test1[[1L]]$results), 7L)
  testthat::expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  testthat::expect_identical(nrow(test2[[1L]]$results), 4L)

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

  testthat::expect_identical(nrow(test1[[1L]]$results), 7L)
  testthat::expect_gt(nrow(test1[[1L]]$results), nrow(test2[[1L]]$results))
  testthat::expect_identical(nrow(test2[[1L]]$results), 4L)


  myEns2 <- caretEnsemble(test2)
  myEns1 <- caretEnsemble(test1)

  testthat::expect_is(myEns2, "caretEnsemble")
  testthat::expect_is(myEns1, "caretEnsemble")
})

testthat::test_that("User tuneTest parameters are respected and model is ensembled", {
  tuneTest <- list(
    rpart = list(method = "rpart", tuneLength = 2L),
    nnet = list(method = "nnet", tuneLength = 2L, trace = FALSE),
    glm = list(method = "glm")
  )
  x <- iris[, 1L:3L]
  y <- iris[, 4L]
  set.seed(42L)
  test_default <- testthat::expect_warning(
    caretList(
      x = x,
      y = y,
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
  set.seed(42L)
  test_flma <- testthat::expect_warning(
    caretList(
      y ~ .,
      data = data.table::data.table(y = y, x),
      tuneList = tuneTest
    ), "There were missing values in resampled performance measures."
  )
  ens_default <- caretEnsemble(test_default)
  ens_flma <- caretEnsemble(test_flma)
  testthat::expect_is(ens_default, "caretEnsemble")
  testthat::expect_is(ens_flma, "caretEnsemble")

  testthat::expect_equal(ens_default$RMSE, ens_flma$RMSE, tol = 0.000001)
  testthat::expect_equal(ens_default$weights, ens_flma$weights, tol = 0.000001)
})

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
  testthat::expect_identical(nrow(pred), nrow(test_data))
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
  testthat::expect_true(all(vapply(models, function(m) "default" %in% colnames(m$results), logical(1L))))
})

testthat::test_that("We can make the stacked predictions matrix", {
  out <- predict(models.reg)
  testthat::expect_s3_class(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L))
  testthat::expect_named(out, c("rf", "glm", "rpart", "treebag"))
})

testthat::test_that("We can predict", {
  out <- predict(models.reg, newdata = X.reg)
  testthat::expect_is(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L))
  testthat::expect_named(out, c("rf", "glm", "rpart", "treebag"))
})

testthat::test_that("We can make the stacked predictions matrix", {
  out <- predict(models.class)
  testthat::expect_s3_class(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L * 1L)) # number of models * (number of classes-1)
})

testthat::test_that("We can predict", {
  out <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  testthat::expect_is(out, "data.table")
  testthat::expect_identical(dim(out), c(150L, 4L * 2L))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  testthat::expect_named(out, paste(combinations$Var2, combinations$Var1, sep = "_"))
  out2 <- predict(models.reg, newdata = X.reg)
  testthat::expect_identical(dim(out2), c(150L, 4L))
  testthat::expect_named(out2, c("rf", "glm", "rpart", "treebag"))
})

testthat::test_that("predict results same regardless of verbose option", {
  invisible(capture.output({
    testthat::expect_is(predict(models.class, newdata = X.class), "data.table")
    out1 <- predict(models.class, newdata = X.class)
    out2 <- predict(models.class, verbose = TRUE, newdata = X.class)
    testthat::expect_identical(out1, out2)

    testthat::expect_is(predict(models.reg, newdata = X.reg), "data.table")
    out1 <- predict(models.reg, newdata = X.reg)
    out2 <- predict(models.reg, verbose = TRUE, newdata = X.reg)
    testthat::expect_identical(out1, out2)
  }))
})

testthat::test_that("plot.caretList", {
  for (model_list in list(models.reg, models.class)) {
    plt <- plot(model_list)
    testthat::expect_is(plt, "ggplot")
    testthat::expect_identical(nrow(plt$data), 4L)
    testthat::expect_named(model_list, plt$data$model_name)
  }
})

testthat::test_that("summary.caretList", {
  for (model_list in list(models.reg, models.class)) {
    smry <- testthat::expect_silent(summary(model_list))
    for (name in names(model_list)) {
      testthat::expect_output(print(smry), name)
    }
  }
})

testthat::test_that("caretList supports combined regression, binary, multiclass", {
  set.seed(42L)

  # Regression models
  reg_models <- caretList(
    Sepal.Length ~ Sepal.Width,
    iris,
    methodList = c("glm", "lm")
  )
  testthat::expect_is(predict(reg_models), "data.table")

  # Binary model
  bin_models <- caretList(
    factor(ifelse(Species == "setosa", "Yes", "No")) ~ Sepal.Width,
    iris,
    methodList = c("lda", "rpart")
  )
  testthat::expect_is(predict(bin_models), "data.table")

  # Multiclass model
  multi_models <- caretList(
    Species ~ Sepal.Width,
    iris,
    methodList = "rpart"
  )
  testthat::expect_is(predict(multi_models), "data.table")

  # Combine them!
  all_models <- c(reg_models, bin_models, multi_models)
  testthat::expect_s3_class(all_models, "caretList")
  testthat::expect_is(vapply(all_models, isClassifierAndValidate, logical(1L)), "logical")

  # Test preds
  stacked_p <- predict(all_models)
  new_p <- predict(all_models, newdata = iris[seq_len(10L), ])
  testthat::expect_is(stacked_p, "data.table")
  testthat::expect_is(new_p, "data.table")
  testthat::expect_identical(nrow(stacked_p), nrow(iris))
  testthat::expect_identical(nrow(new_p), 10L)
})

testthat::test_that("Stacked predictions works on new model types", {
  # Note that new model types would have to return a single column called 'pred'
  models.class.new <- models.reg
  for (idx in seq_along(models.class.new)) {
    models.class.new[[idx]]$modelType <- "TimeSeries"
  }
  preds <- predict(models.class.new)
  testthat::expect_s3_class(preds, "data.table")
})

testthat::test_that("Stacked predictions creates prediction-observation data correctly", {
  stacked_preds_class <- predict(models.class)
  stacked_preds_reg <- predict(models.reg)

  testthat::expect_s3_class(stacked_preds_class, "data.table")
  testthat::expect_s3_class(stacked_preds_reg, "data.table")

  testthat::expect_identical(ncol(stacked_preds_class), length(models.class))
  testthat::expect_identical(ncol(stacked_preds_reg), length(models.reg))

  testthat::expect_named(stacked_preds_class, names(models.class))
  testthat::expect_named(stacked_preds_reg, names(stacked_preds_reg))

  testthat::expect_identical(nrow(stacked_preds_class), 150L)
  testthat::expect_identical(nrow(stacked_preds_reg), 150L)
})
