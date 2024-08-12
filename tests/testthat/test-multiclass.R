utils::data(iris)
utils::data(Boston, package = "MASS")

model_list <- caretList(
  iris[, -5L],
  iris[, 5L],
  tuneLength = 1L,
  methodList = c("glmnet", "rpart"),
  tuneList = list(
    nnet = caretModelSpec(method = "nnet", trace = FALSE)
  )
)

######################################################################
testthat::context("caretList and caretStack work for multiclass problems")
######################################################################

testthat::test_that("We can predict with caretList and caretStack for multiclass problems", {
  p <- predict(model_list, newdata = iris[, -5L])
  testthat::expect_s3_class(p, "data.table")
  testthat::expect_identical(nrow(p), nrow(iris))

  ens <- caretStack(model_list, method = "rpart")

  p_raw <- predict(ens, newdata = iris[, -5L])
  testthat::expect_s3_class(p_raw, "data.table")
  testthat::expect_identical(nrow(p_raw), nrow(iris))

  p <- predict(ens, newdata = iris[, -5L])
  testthat::expect_s3_class(p, "data.table")
  testthat::expect_identical(nrow(p), nrow(iris))
})

testthat::test_that("Columns for caretList predictions are correct and ordered", {
  num_methods <- length(model_list)
  num_classes <- length(unique(iris$Species))

  p <- predict(model_list, newdata = iris[, -5L], excluded_class_id = 0L)
  testthat::expect_identical(dim(p), c(nrow(iris), num_methods * num_classes))

  methods <- names(model_list)
  classes <- levels(iris$Species)
  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))

  testthat::expect_true(all(colnames(p) %in% ordered_colnames))
  testthat::expect_named(p, ordered_colnames)
})

testthat::test_that("Columns for caretStack are correct", {
  model_stack <- caretStack(model_list, method = "knn")

  num_classes <- length(unique(iris$Species))
  classes <- levels(iris$Species)

  p_raw <- predict(model_stack, newdata = iris[, -5L])
  testthat::expect_identical(nrow(p_raw), nrow(iris))

  p_prob <- predict(model_stack, newdata = iris[, -5L])
  testthat::expect_identical(dim(p_prob), c(nrow(iris), num_classes))
  testthat::expect_named(p_prob, classes)
})

testthat::test_that("Periods are supported in method and class names in caretList and caretStack", {
  iris_mod <- iris
  levels(iris_mod[, 5L]) <- c("setosa_1", "versicolor_2", "virginica_3")
  iris_mod[, 5L] <- factor(iris_mod[, 5L])

  model_list <- caretList(
    x = iris_mod[, -5L],
    y = iris_mod[, 5L],
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet_1 = caretModelSpec(
        method = "nnet",
        tuneGrid = expand.grid(.size = c(1L, 3L, 5L), .decay = 0.3),
        trace = FALSE
      ),
      nnet_2 = caretModelSpec(
        method = "nnet",
        tuneGrid = expand.grid(.size = 3L, .decay = c(0.1, 0.3, 0.5)),
        trace = FALSE
      )
    )
  )

  methods <- names(model_list)
  classes <- levels(iris_mod[, 5L])

  p <- predict(model_list, newdata = iris_mod[, -5L], excluded_class_id = 0L)
  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  testthat::expect_named(p, ordered_colnames)

  model_stack <- caretStack(model_list, method = "knn")
  p_prob <- predict(model_stack, newdata = iris_mod[, -5L])
  testthat::expect_named(p_prob, classes)
  p_raw <- predict(model_stack, newdata = iris_mod[, -5L])
  testthat::expect_named(p_raw, classes)
})

testthat::test_that("We can make a confusion matrix", {
  model_stack <- caretStack(model_list, method = "knn")

  predictions <- predict(model_stack, newdata = iris[, -5L], return_class_only = TRUE)
  cm <- caret::confusionMatrix(predictions, iris[, 5L])

  testthat::expect_s3_class(cm, "confusionMatrix")
  testthat::expect_identical(dim(cm$table), c(3L, 3L))
  testthat::expect_gt(cm$overall["Accuracy"], 0.95) # In sample accuracy should be high
})

testthat::test_that("caretList and caretStack handle imbalanced multiclass data", {
  set.seed(123L)
  n <- 1000L
  X <- data.table::data.table(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  y <- factor(c(rep("A", 700L), rep("B", 200L), rep("C", 100L)))

  model_list <- caretList(X, y, methodList = "rpart")
  testthat::expect_s3_class(model_list, "caretList")
  testthat::expect_length(model_list, 1L)

  stack <- caretStack(model_list, method = "glmnet")
  testthat::expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = X)
  testthat::expect_named(preds, levels(y))
})

testthat::test_that("caretList and caretStack handle a large number of classes", {
  set.seed(123L)
  n <- 1000L
  X <- data.table::data.table(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  y <- factor(sample(paste0("Class", 1L:100L), n, replace = TRUE))

  model_list <- caretList(X, y, methodList = "rpart")
  testthat::expect_s3_class(model_list, "caretList")

  stack <- caretStack(model_list, method = "rpart")
  testthat::expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = X)
  testthat::expect_identical(ncol(preds), nlevels(y))
})

testthat::test_that("caretList and caretStack handle ordinal multiclass data", {
  Boston$chas <- as.factor(Boston$chas)
  Boston$rad <- factor(paste0("rad_", Boston$rad), ordered = TRUE)

  model_list <- caretList(rad ~ ., data = Boston, methodList = c("rpart", "glmnet"))
  testthat::expect_s3_class(model_list, "caretList")

  stack <- caretStack(model_list, method = "rpart")
  testthat::expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = Boston)
  testthat::expect_s3_class(preds, "data.table")
  testthat::expect_named(preds, levels(Boston$rad))
  testthat::expect_equal(rowSums(preds), rep(1.0, nrow(Boston)), tolerance = 1e-4)
})

testthat::test_that("caretList and caretStack produce consistent probability predictions", {
  stack <- caretStack(model_list, method = "rpart")

  prob_preds <- predict(stack, newdata = iris[, -5L])
  testthat::expect_identical(nrow(prob_preds), nrow(iris))
  testthat::expect_identical(ncol(prob_preds), nlevels(iris$Species))
  testthat::expect_true(all(rowSums(prob_preds) >= 0.99))
  testthat::expect_true(all(rowSums(prob_preds) <= 1.01))
})

testthat::test_that("caretList and caretStack handle new levels in prediction data", {
  set.seed(123L)
  idx <- seq_len(nrow(iris))
  idx_train <- sample(idx, 120L)
  idx_test <- setdiff(idx, idx_train)
  train_data <- iris[idx_train, ]
  test_data <- iris[idx_test, ]
  test_data$Species <- factor(as.character(test_data$Species), levels = c(levels(iris$Species), "NewSpecies"))
  test_data$Species[1L] <- "NewSpecies"

  model_list <- caretList(train_data[, -5L], train_data[, 5L], methodList = c("rf", "rpart"))
  stack <- caretStack(model_list, method = "rpart")

  preds <- predict(stack, newdata = test_data)
  testthat::expect_true(all(colnames(preds) %in% levels(train_data$Species)))
})
