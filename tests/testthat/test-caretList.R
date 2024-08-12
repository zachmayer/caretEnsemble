# Setup
set.seed(442L)

utils::data(models.reg)
utils::data(X.reg)
utils::data(Y.reg)

utils::data(models.class)
utils::data(X.class)
utils::data(Y.class)

train <- caret::twoClassSim(
  n = 1000L, intercept = -8L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)
test <- caret::twoClassSim(
  n = 1500L, intercept = -7L, linearVars = 3L,
  noiseVars = 10L, corrVars = 4L, corrValue = 0.6
)

n <- 100L
p <- 1000L
large_data <- list(
  X = data.table::data.table(matrix(stats::rnorm(n * p), n, p)),
  y = factor(sample(c("A", "B"), n, replace = TRUE))
)

################################################################
testthat::context("caretModelSpec, tuneCheck, methodCheck")
################################################################
testthat::test_that("caretModelSpec and checking functions work as expected", {
  all_models <- sort(unique(caret::modelLookup()$model))

  testthat::expect_identical(caretModelSpec("rf", tuneLength = 5L, preProcess = "knnImpute")$method, "rf")

  methodList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  all_models_check <- tuneCheck(methodList)
  testthat::expect_is(all_models_check, "list")
  testthat::expect_length(all_models, length(all_models_check))

  methodCheck(all_models)
  testthat::expect_error(
    methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL")),
    "The following models are not valid caret models: THIS_IS_NOT_A_REAL_MODEL"
  )

  testthat::expect_error(
    methodCheck("InvalidMethod"),
    "The following models are not valid caret models: InvalidMethod"
  )

  testthat::expect_error(
    methodCheck(list(invalid_method = 42L)),
    "Method \"42\" is invalid"
  )
})

################################################################
testthat::context("S3 methods for caretlist")
################################################################

testthat::test_that("Target extraction functions work", {
  data(iris)
  testthat::expect_identical(extractCaretTarget(iris[, 1L:4L], iris[, 5L]), iris[, 5L])
  testthat::expect_identical(extractCaretTarget(Species ~ ., iris), iris[, "Species"])
})

testthat::test_that("[.caretList", {
  subset_models <- models.class[1L:2L]
  testthat::expect_s3_class(subset_models, "caretList")
  testthat::expect_length(subset_models, 2L)
})

testthat::test_that("c.caretList", {
  combined_models <- c(models.class, models.class)
  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, length(models.class) * 2L)

  combined_models <- c(models.class, models.class[[1L]])
  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, length(models.class) + 1L)

  testthat::expect_error(c.caretList(list(a = 1L, b = 2L)), "class of modelList1 must be 'caretList' or 'train'")
})

testthat::test_that("as.caretList", {
  # Named
  model_list <- list(model1 = models.class[[1L]], model2 = models.class[[2L]])
  caretlist_object <- as.caretList(model_list)
  testthat::expect_s3_class(caretlist_object, "caretList")
  testthat::expect_length(caretlist_object, 2L)

  # Unnamed
  model_list <- list(models.class[[1L]], models.class[[2L]])
  caretlist_object <- as.caretList(model_list)
  testthat::expect_s3_class(caretlist_object, "caretList")
  testthat::expect_length(caretlist_object, 2L)

  # Error cases
  testthat::expect_error(as.caretList(NULL), "object is null")
  testthat::expect_error(as.caretList(1L), "object must be a list")
  testthat::expect_error(as.caretList(list(1L)), "object requires all elements of list to be caret models")
  testthat::expect_error(as.caretList(list(NULL)), "object requires all elements of list to be caret models")
  testthat::expect_error(as.caretList.list(1L), "object must be a list of caret models")
})

################################################################
testthat::context("predict.caretlist")
################################################################

testthat::test_that("predict.caretList works for classification and regression", {
  class_preds <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  reg_preds <- predict(models.reg, newdata = X.reg)

  testthat::expect_is(class_preds, "data.table")
  testthat::expect_is(reg_preds, "data.table")
  testthat::expect_identical(nrow(class_preds), nrow(X.class))
  testthat::expect_identical(nrow(reg_preds), nrow(X.reg))

  # Test for handling new factor levels in prediction
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

  # Test verbose option
  p <- predict(models, newdata = test_data, verbose = TRUE)
  testthat::expect_s3_class(p, "data.table")
  testthat::expect_identical(nrow(p), nrow(test_data))
})

################################################################
testthat::context("caretList")
################################################################

testthat::test_that("caretList works for various scenarios", {
  # Basic classification
  test1 <- caretList(
    x = train[, -23L],
    y = train[, "Class"],
    methodList = c("knn", "glm")
  )
  testthat::expect_is(test1, "caretList")
  testthat::expect_is(caretEnsemble(test1), "caretEnsemble")

  # Regression
  test_reg <- caretList(
    x = train[, c(-23L, -1L)],
    y = train[, 1L],
    methodList = c("glm", "lm")
  )
  testthat::expect_is(test_reg, "caretList")
  testthat::expect_is(caretEnsemble(test_reg), "caretEnsemble")

  # Handling missing data
  iris_with_na <- iris
  x <- iris_with_na[, 1L:4L]
  y <- iris_with_na[, 5L]
  x[sample.int(nrow(x), 10L), sample.int(ncol(x), 2L)] <- NA
  models <- caretList(x = x, y = y, methodList = "rpart")
  testthat::expect_s3_class(models, "caretList")

  # Handling large number of predictors
  models_large <- caretList(x = large_data$X, y = large_data$y, methodList = "rpart")
  testthat::expect_s3_class(models_large, "caretList")
  testthat::expect_length(models_large, 1L)

  # Handling imbalanced data
  imbalanced_y <- factor(c(rep("A", 95L), rep("B", 5L)))
  testthat::expect_length(imbalanced_y, nrow(large_data$X))
  models_imbalanced <- caretList(
    x = large_data$X,
    y = imbalanced_y,
    methodList = "rpart",
    trControl = caret::trainControl(
      method = "cv",
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "up",
      index = caret::createFolds(imbalanced_y, k = 5L, returnTrain = TRUE)
    )
  )
  testthat::expect_s3_class(models_imbalanced, "caretList")
  testthat::expect_length(models_imbalanced, 1L)

  # Test error cases
  testthat::expect_error(caretList(Sepal.Width ~ ., iris), "Please either define a methodList or methodList")
  testthat::expect_warning(
    caretList(Sepal.Width ~ ., iris, methodList = c("lm", "lm")),
    "Duplicate entries in methodList. Using unique methodList values."
  )

  # Test continue_on_fail
  bad <- list(
    bad = caretModelSpec(method = "glm", tuneLength = 1L)
  )
  testthat::expect_output(
    testthat::expect_warning(
      testthat::expect_error(
        caretList(iris[, 1L:4L], iris[, 5L], methodList = bad, continue_on_fail = TRUE),
        regexp = "caret:train failed for all models. Please inspect your data."
      ),
      regexp = "model fit failed for Fold1"
    ),
    regexp = "Something is wrong; all the Accuracy metric values are missing:"
  )
})

# Test plot and summary methods
testthat::test_that("plot.caretList and summary.caretList work", {
  for (model_list in list(models.reg, models.class)) {
    plt <- plot(model_list)
    testthat::expect_is(plt, "ggplot")
    testthat::expect_identical(nrow(plt$data), 4L)
    testthat::expect_named(model_list, plt$data$model_name)

    smry <- testthat::expect_silent(summary(model_list))
    for (name in names(model_list)) {
      testthat::expect_output(print(smry), name)
    }
  }
})

# Test combined regression, binary, multiclass models
testthat::test_that("caretList supports combined regression, binary, multiclass", {
  set.seed(42L)

  reg_models <- caretList(Sepal.Length ~ Sepal.Width, iris, methodList = c("glm", "lm"))
  bin_models <- caretList(factor(ifelse(Species == "setosa", "Yes", "No")) ~ Sepal.Width, iris,
    methodList = c("lda", "rpart")
  )
  multi_models <- caretList(Species ~ Sepal.Width, iris, methodList = "rpart")

  all_models <- c(reg_models, bin_models, multi_models)
  testthat::expect_s3_class(all_models, "caretList")

  stacked_p <- predict(all_models)
  new_p <- predict(all_models, newdata = iris[1L:10L, ])
  testthat::expect_is(stacked_p, "data.table")
  testthat::expect_is(new_p, "data.table")
  testthat::expect_identical(nrow(stacked_p), nrow(iris))
  testthat::expect_identical(nrow(new_p), 10L)
})

testthat::test_that("caretList supports custom models", {
  set.seed(42L)

  # Use the custom greedyMSE model
  custom_list <- list(
    custom.mse = caretModelSpec(method = greedyMSE_caret(), tuneLength = 1L)
  )

  # Fit it reg/bin/multi (it supports all 3!)
  reg_models <- caretList(Sepal.Length ~ Sepal.Width, iris, methodList = custom_list)
  bin_models <- caretList(factor(ifelse(Species == "setosa", "Y", "N")) ~ Sepal.Width, iris, methodList = custom_list)
  multi_models <- caretList(Species ~ Sepal.Width, iris, methodList = custom_list)

  # Check the fit
  all_models <- c(reg_models, bin_models, multi_models)
  testthat::expect_s3_class(all_models, "caretList")

  # Check predictions
  stacked_p <- predict(all_models)
  new_p <- predict(all_models, newdata = iris[1L:10L, ])
  testthat::expect_is(stacked_p, "data.table")
  testthat::expect_is(new_p, "data.table")
  testthat::expect_identical(nrow(stacked_p), nrow(iris))
  testthat::expect_identical(nrow(new_p), 10L)

  # Check we can stack it
  # Note that caretStack with method=greedyMSE_caret()
  # is what caretEnsemble does under the hood
  ens <- caretStack(
    all_models,
    method = greedyMSE_caret(),
    trControl = trainControl(method = "cv", number = 2L, savePredictions = "final")
  )
  stacked_p <- predict(ens)
  new_p <- predict(ens, newdata = iris[1L:10L, ])
  testthat::expect_is(stacked_p, "data.table")
  testthat::expect_is(new_p, "data.table")
  testthat::expect_identical(nrow(stacked_p), nrow(iris))
  testthat::expect_identical(nrow(new_p), 10L)
})

################################################################
testthat::context("caretModelSpec")
################################################################

testthat::test_that("caretModelSpec works correctly", {
  # Test valid input
  testthat::expect_s3_class(caretModelSpec("rf"), "caretModelSpec")
  testthat::expect_identical(caretModelSpec("rf", tuneLength = 5L)$tuneLength, 5L)

  # Test invalid method
  testthat::expect_error(
    caretModelSpec("invalid_method"),
    "'invalid_method' not supported"
  )

  # Test non-character method
  testthat::expect_error(caretModelSpec(123L), "is.character(method) is not TRUE", fixed = TRUE)
})

testthat::test_that("validateMethodList works correctly", {
  # Test character vector input
  char_vector <- c("rf", "glm")
  model_list <- validateMethodList(char_vector)
  testthat::expect_type(model_list, "list")
  testthat::expect_length(model_list, 2L)
  testthat::expect_true(all(unlist(lapply(model_list, inherits, "caretModelSpec"))))
  testthat::expect_named(model_list, char_vector)

  # List input
  model_list <- validateMethodList(list(rf = caretModelSpec("rf"), glm = caretModelSpec("glm")))
  testthat::expect_type(model_list, "list")
  testthat::expect_length(model_list, 2L)
  testthat::expect_true(all(unlist(lapply(model_list, inherits, "caretModelSpec"))))
  testthat::expect_named(model_list, c("rf", "glm"))

  # Mixed input
  model_list <- validateMethodList(list("rf", glm = caretModelSpec("glm")))
  testthat::expect_type(model_list, "list")
  testthat::expect_length(model_list, 2L)
  testthat::expect_true(all(unlist(lapply(model_list, inherits, "caretModelSpec"))))
  testthat::expect_named(model_list, c("rf", "glm"))

  # No names input
  model_list <- validateMethodList(list("rf", caretModelSpec("glm")))
  testthat::expect_type(model_list, "list")
  testthat::expect_length(model_list, 2L)
  testthat::expect_true(all(unlist(lapply(model_list, inherits, "caretModelSpec"))))
  testthat::expect_named(model_list, c("rf", "glm"))

  # Custom models
  custom_model_specs <- lapply(caret::getModelInfo("^glm$|^rf$"), function(x) caretModelSpec(method = x))
  model_list <- validateMethodList(custom_model_specs)
  testthat::expect_type(model_list, "list")
  testthat::expect_length(model_list, 2L)
  testthat::expect_true(all(unlist(lapply(model_list, inherits, "caretModelSpec"))))
  testthat::expect_named(model_list, c("glm", "rf"))
})

testthat::test_that("validateMethodList errors", {
  testthat::expect_error(validateMethodList(NULL), "Please define a methodList")
  testthat::expect_error(validateMethodList(123L), "is.list(methodList) is not TRUE", fixed = TRUE)
  testthat::expect_error(validateMethodList(list("invalid_model")), "'invalid_model' not supported. ", fixed = TRUE)
  testthat::expect_error(validateMethodList(list(rf = "invalid_model")), "'invalid_model' not supported. ")
})

testthat::test_that("validateMethodList handles duplicates correctly", {
  duplicate_list <- list(rf = caretModelSpec("rf"), caretModelSpec("rf"))
  unique_list <- validateMethodList(duplicate_list)
  testthat::expect_named(unique_list, c("rf.1", "rf.2"))

  duplicate_list <- c("rf", "rf")
  unique_list <- validateMethodList(duplicate_list)
  testthat::expect_named(unique_list, c("rf.1", "rf.2"))

  duplicate_list <- list("rf", rf = caretModelSpec("rf"))
  unique_list <- validateMethodList(duplicate_list)
  testthat::expect_named(unique_list, c("rf.1", "rf.2"))
})
