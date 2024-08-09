# Setup
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

n <- 100L
p <- 1000L
large_data <- list(
  X = data.table::data.table(matrix(stats::rnorm(n * p), n, p)),
  y = factor(sample(c("A", "B"), n, replace = TRUE))
)


# Test caretModelSpec, tuneCheck, methodCheck
testthat::test_that("caretModelSpec and checking functions work as expected", {
  all_models <- sort(unique(caret::modelLookup()$model))

  testthat::expect_identical(caretModelSpec("rf", tuneLength = 5L, preProcess = "knnImpute")$method, "rf")

  tuneList <- lapply(all_models, function(x) list(method = x, preProcess = "pca"))
  all_models_check <- tuneCheck(tuneList)
  testthat::expect_is(all_models_check, "list")
  testthat::expect_length(all_models, length(all_models_check))

  methodCheck(all_models)
  testthat::expect_error(
    methodCheck(c(all_models, "THIS_IS_NOT_A_REAL_MODEL")),
    "The following models are not valid caret models: THIS_IS_NOT_A_REAL_MODEL"
  )
})

# Test extractCaretTarget
testthat::test_that("Target extraction functions work", {
  data(iris)
  testthat::expect_identical(extractCaretTarget(iris[, 1L:4L], iris[, 5L]), iris[, 5L])
  testthat::expect_identical(extractCaretTarget(Species ~ ., iris), iris[, "Species"])
})

# Test S3 methods for caretList
testthat::test_that("S3 methods for caretList work correctly", {
  subset_models <- models.class[1L:2L]
  testthat::expect_s3_class(subset_models, "caretList")
  testthat::expect_length(subset_models, 2L)

  models_class1 <- models.class[1L:2L]
  models_class2 <- models.class[3L:4L]
  class(models_class1) <- class(models_class2) <- "caretList"
  combined_models <- c(models_class1, models_class2)
  testthat::expect_s3_class(combined_models, "caretList")
  testthat::expect_length(combined_models, length(models.class))

  model_list <- list(model1 = models.class[[1L]], model2 = models.class[[2L]])
  caretlist_object <- as.caretList(model_list)
  testthat::expect_s3_class(caretlist_object, "caretList")
  testthat::expect_length(caretlist_object, 2L)
})

# Test predict.caretList
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
})

# Test caretList
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
