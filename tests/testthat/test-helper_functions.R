suppressMessages({
  library(testthat)
  library(caret)
  library(rpart)
})

########################################################################
context("Do the helper functions work for regression objects?")
########################################################################

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

test_that("Recycling generates a warning", {
  expect_error(caretEnsemble:::wtd.sd(matrix(1:10, ncol = 2), w = 1))
})

test_that("No predictions generates an error", {
  suppressWarnings(
    models_multi <- caretList(
      iris[, 1:2], iris[, 5],
      tuneLength = 1, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
    )
  )
  check_caretList_model_types(models_multi)

  suppressWarnings(
    models <- caretList(
      iris[, 1:2], factor(ifelse(iris[, 5] == "setosa", "Yes", "No")),
      tuneLength = 1, verbose = FALSE,
      methodList = c("rf", "gbm"),
      trControl = trainControl(method = "cv", number = 2, savePredictions = "final", classProbs = TRUE)
    )
  )
  new_model <- train(
    iris[, 1:2], factor(ifelse(iris[, 5] == "setosa", "Yes", "No")),
    tuneLength = 1,
    method = c("glmnet"),
    trControl = trainControl(method = "cv", number = 2, savePredictions = "none", classProbs = TRUE)
  )
  models2 <- c(new_model, models)
  models3 <- c(models, new_model)
  check_caretList_model_types(models)
  expect_error(check_caretList_model_types(models2))
  expect_error(check_caretList_model_types(models3))
})

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.reg)
  expect_is(out, "list")
  expect_true(length(out$obs) == 150)
  expect_true(all(dim(out$preds) == c(150, 4)))
})

test_that("We can predict", {
  out <- predict(models.reg, "reg", newdata = X.reg)
  expect_is(out, "matrix")
  expect_true(all(dim(out) == c(150, 4)))
  expect_true(all(colnames(out) == c("rf", "glm", "rpart", "treebag")))
})

########################################################################
context("Do the helper functions work for classification objects?")
########################################################################

test_that("We can make the predobs matrix", {
  out <- makePredObsMatrix(models.class)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs) == 150)
  expect_true(all(dim(out$preds) == c(150, 4 * 1))) # number of models * (number of classes-1)
})

test_that("We can predict", {
  out <- predict(models.class, "Classification", newdata = X.class)
  expect_is(out, "matrix")
  expect_true(all(dim(out) == c(150, 4 * 2)))
  model_names <- c("rf", "glm", "rpart", "treebag")
  class_names <- c("No", "Yes")
  combinations <- expand.grid(class_names, model_names)
  expect_true(all(colnames(out) == paste(combinations$Var2, combinations$Var1, sep = "_")))
  out2 <- predict(models.reg, "Regression", newdata = X.reg)
  expect_true(all(dim(out2) == c(150, 4)))
  expect_true(all(colnames(out2) == c("rf", "glm", "rpart", "treebag")))
})

test_that("predict results same regardless of verbose option", {
  invisible(capture.output({
    suppressWarnings({
      expect_is(predict(models.class, "Classification", newdata = X.class), "matrix")
      out1 <- predict(models.class, "Classification", newdata = X.class)
      out2 <- predict(models.class, "Classification", verbose = TRUE, newdata = X.class)
      expect_identical(out1, out2)
    })

    expect_is(predict(models.reg, "Regression", newdata = X.reg), "matrix")
    out1 <- predict(models.reg, "Regression", newdata = X.reg)
    out2 <- predict(models.reg, "Regression", verbose = TRUE, newdata = X.reg)
    expect_identical(out1, out2)
  }))
})

context("Test weighted standard deviations")

x <- rnorm(1000)
x1 <- c(3, 5, 9, 3, 4, 6, 4)
x2 <- c(10, 10, 20, 14, 2, 2, 40)
y <- c(10, 10, 10, 20)
w1 <- c(0.1, 0.1, 0.1, 0.7)

test_that("wtd.sd applies weights correctly", {
  expect_error(caretEnsemble:::wtd.sd(x))
  expect_false(sd(x1) == caretEnsemble:::wtd.sd(x1, w = x2))
  expect_false(sd(x1) == caretEnsemble:::wtd.sd(x1, w = x2))
  expect_equal(caretEnsemble:::wtd.sd(y, w = w1), 7.84, tolerance = .001)
  expect_equal(caretEnsemble:::wtd.sd(y, w = w1 * 100), caretEnsemble:::wtd.sd(y, w = w1))
})

test_that("wtd.sd handles NA values correctly", {
  y <- c(10, 10, 10, 20, NA, NA)
  w1 <- c(0.1, 0.1, 0.1, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1)))
  expect_true(is.na(sd(y)))
  expect_true(!is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm = TRUE)))
  expect_true(!is.na(sd(y, na.rm = TRUE)))
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1)))
  expect_true(!is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm = TRUE)))
  w2 <- c(0.1, 0.1, NA, 0.7, NA, NA)
  expect_true(is.na(caretEnsemble:::wtd.sd(y, w = w1, na.rm = TRUE) == caretEnsemble:::wtd.sd(y, w = w2, na.rm = TRUE)))
})

test_that("Checks generate errors", {
  skip_on_cran()
  set.seed(42)
  myControl <- trainControl(method = "cv", number = 5, savePredictions = "final")
  expect_warning(
    x <- caretList(
      Sepal.Length ~ Sepal.Width,
      iris,
      methodList = c("glm", "lm"),
      trControl = myControl
    )
  )
  modelLibrary <- extractBestPreds(x)
  modelLibrary$nn <- modelLibrary$lm[sample(1:nrow(modelLibrary$lm), nrow(modelLibrary$lm)), ]

  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_warning(x$rpart <- train(Sepal.Length ~ Sepal.Width, iris, method = "rpart"))
  expect_error(check_bestpreds_resamples(modelLibrary))
  expect_error(check_bestpreds_indexes(modelLibrary))
  expect_error(check_bestpreds_obs(modelLibrary))

  expect_error(check_caretList_classes(x$glm$finalModel))

  x$rpart <- train(Species ~ Sepal.Width, iris, method = "rpart", trControl = myControl)
  # This has to be changed because train gives this error if dataset is truncated in
  # newest version of caret:
  # Error in train.default(x, y, weights = w, ...) :
  #    One or more factor levels in the outcome has no data: 'virginica'
  check_caretList_classes(x)
  expect_error(check_caretList_model_types(x))

  m <- extractBestPreds(x)
  expect_error(check_bestpreds_preds(m))

  set.seed(42)
  myControl2 <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  suppressWarnings(
    x <- caretList(
      iris[1:100, -5],
      factor(ifelse(iris[1:100, "Species"] == "setosa", "Yes", "No")),
      methodList = c("lda", "rf"),
      trControl = myControl2
    )
  )
  x$rpart <- train(Species ~ Sepal.Width + Sepal.Length, iris, method = "rpart")
  expect_error(check_caretList_model_types(x))
})

test_that("check_caretList_model_types stops when there are no predictions saved", {
  model_list <- models.class
  model_list[[1]]$pred <- NULL
  expect_error(check_caretList_model_types(model_list), "No predictions saved by train. Please re-run models with trainControl set with savePredictions = TRUE.")
})

test_that("check_caretList_model_types stops when a classification model support probabilities", {
  model_list <- models.class
  model_list[[1]]$modelInfo$prob <- FALSE
  expect_error(check_caretList_model_types(model_list), "All models for classification must be able to generate class probabilities.")
})

test_that("check_caretList_model_types stops when a classification model supports probabilities but did not save them", {
  model_list <- models.class
  model_list[[1]]$control$classProbs <- FALSE
  m <- "Some models were fit with no class probabilities. Please re-fit them with trainControl, classProbs=TRUE"
  expect_error(check_caretList_model_types(model_list), m)
  context("Test helper functions for multiclass classification")

  test_that("Check errors in caretEnsemble for multiclass classification work", {
    skip_on_cran()
    data(iris)
    myControl <- trainControl(method = "cv", number = 5, savePredictions = "final", index = createResample(iris[, 5], 5))
    model_list <- caretList(
      x = iris[, -5],
      y = iris[, 5],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    expect_error(check_binary_classification(model_list))
    expect_true(is.null(check_binary_classification(models.class)))
    expect_true(is.null(check_binary_classification(models.reg)))

    # Do not produce errors when another object is passed
    expect_true(is.null(check_binary_classification(NULL)))
    expect_true(is.null(check_binary_classification(2)))
    expect_true(is.null(check_binary_classification(list("string"))))
    expect_true(is.null(check_binary_classification(iris)))
  })

  test_that("Configuration function for excluded level work", {
    expect_warning(check_multiclass_excluded_level(4, 3))
    expect_warning(check_multiclass_excluded_level(0, 3))
    expect_true(is.null(check_multiclass_excluded_level(3, 3)))
    expect_true(is.null(check_multiclass_excluded_level(1, 3)))

    data(iris)
    myControl <- trainControl(
      method = "cv", number = 5,
      savePredictions = "final", index = createResample(iris[, 5], 5),
      classProbs = TRUE
    )
    model_list <- caretList(
      x = iris[, -5],
      y = iris[, 5],
      methodList = c("rpart", "glmnet"),
      trControl = myControl
    )

    msg <- "multiclass excluded level must be > 0: 0 was given see setMulticlassExcludedLevel for more details"
    expect_error(setMulticlassExcludedLevel(0L), msg)
    invisible(caretStack(model_list, method = "knn"))
    setMulticlassExcludedLevel(4L)
    expect_warning(caretStack(model_list, method = "knn"))
    setMulticlassExcludedLevel(1L)

    # Check if we are actually excluding level 1 (setosa)
    setMulticlassExcludedLevel(1L)
    classes <- levels(iris[, 5])[-1]
    models <- c("rpart", "glmnet")
    class_model_combinations <- expand.grid(classes, models)
    varImp_rownames <- apply(class_model_combinations, 1, function(x) paste(x[2], x[1], sep = "_"))

    model_stack <- caretStack(model_list, method = "knn")
    expect_identical(rownames(varImp(model_stack$ens_model)$importance), varImp_rownames)
  })
})

# Tests for validateMulticlassExcludedLevel function
test_that("validateMulticlassExcludedLevel stops for non-numeric input", {
  invalid_input <- "invalid"
  err <- "multiclass excluded level must be numeric: invalid was given see setMulticlassExcludedLevel for more details"
  expect_error(validateMulticlassExcludedLevel(invalid_input), err)
})

test_that("validateMulticlassExcludedLevel stops for non-finite input", {
  invalid_input <- Inf
  err <- "multiclass excluded level must be finite: Inf was given see setMulticlassExcludedLevel for more details"
  expect_error(validateMulticlassExcludedLevel(invalid_input), err)
})

test_that("validateMulticlassExcludedLevel stops for non-positive input", {
  invalid_input <- -1
  err <- "multiclass excluded level must be > 0: -1 was given see setMulticlassExcludedLevel for more details"
  expect_error(validateMulticlassExcludedLevel(invalid_input), err)
})

test_that("validateMulticlassExcludedLevel warns for non-integer input", {
  warn <- "multiclass excluded level is not an integer 1.1 was given see setMulticlassExcludedLevel for more details"
  expect_warning(
    {
      validated <- validateMulticlassExcludedLevel(1.1)
    },
    warn
  )
  expect_equal(validated, 1L)
})

test_that("validateMulticlassExcludedLevel passes for valid input", {
  valid_input <- 3L
  expect_equal(validateMulticlassExcludedLevel(valid_input), 3L)
})
