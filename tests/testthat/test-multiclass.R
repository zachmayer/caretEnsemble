#############################################################################
context("caretList and caretStack work for multiclass problems")
#############################################################################
test_that("We can predict with caretList and caretStack multiclass problems", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5L], 5L)
  )
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    trControl = my_control,
    methodList = c("glmnet", "rpart")
  )

  p <- predict(model_list, newdata = iris[, -5L])
  expect_is(p, "data.table")
  expect_equal(nrow(p), nrow(iris))

  ens <- caretStack(model_list, method = "rpart")

  p_raw <- predict(ens, newdata = iris[, -5L])
  expect_s3_class(p_raw, "data.table")
  expect_identical(nrow(p_raw), nrow(iris))

  p <- predict(ens, newdata = iris[, -5L])
  expect_is(p, "data.frame")
  expect_equal(nrow(p), nrow(iris))
})

test_that("Columns for caretList predictions are correct and ordered", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5L], 5L)
  )
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  num_methods <- length(model_list)
  num_classes <- length(unique(iris$Species))

  # Check the number of rows and columns is correct
  p <- predict(model_list, newdata = iris[, -5L], excluded_class_id = 0L)
  expect_equal(dim(p), c(nrow(iris), num_methods * num_classes))

  methods <- names(model_list)
  classes <- levels(iris$Species)
  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))

  # Check the names of the columns are correct
  expect_true(all(colnames(p) %in% ordered_colnames))

  # Check that the columns are ordered correctly
  expect_equal(colnames(p), ordered_colnames)
})

test_that("Columns for caretStack are correct", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5L], 5L)
  )
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    trControl = my_control,
    methodList = "rpart",
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  model_stack <- caretStack(model_list, method = "knn")

  num_classes <- length(unique(iris$Species))

  # Check the number of rows and columns is correct
  p_raw <- predict(model_stack, newdata = iris[, -5L])
  expect_identical(nrow(p_raw), nrow(iris))
  p_prob <- predict(model_stack, newdata = iris[, -5L])
  expect_equal(dim(p_prob), c(nrow(iris), num_classes))

  classes <- levels(iris$Species)

  # Check that the columns are ordered correctly
  expect_named(p_prob, classes)
})

test_that("Periods are supported in method and class names in caretList and caretStack", {
  data(iris)
  # Rename values and levels to have underscores
  levels(iris[, 5L]) <- c("setosa_1", "versicolor_2", "virginica_3")
  iris[, 5L] <- factor(iris[, 5L])

  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5L], 5L)
  )
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    trControl = my_control,
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
  classes <- levels(iris[, 5L])

  p <- predict(model_list, newdata = iris[, -5L], excluded_class_id = 0L)

  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1L, function(x) paste(x[2L], x[1L], sep = "_"))
  expect_equal(colnames(p), ordered_colnames)

  model_stack <- caretStack(model_list, method = "knn", trControl = trainControl(
    savePredictions = "final", classProbs = TRUE
  ))
  p_prob <- predict(model_stack, newdata = iris[, -5L])
  expect_equal(colnames(p_prob), classes)
  p_raw <- predict(model_stack, newdata = iris[, -5L])
  expect_named(p_raw, classes)
})

test_that("We can make a confusion matrix", {
  data(iris)

  set.seed(42L)
  n <- nrow(iris)
  train_indices <- sample.int(n, n * 0.8)
  train_data <- iris[train_indices, ]
  test_data <- iris[-train_indices, ]

  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(train_data[, 5L], 5L)
  )
  model_list <- caretList(
    x = train_data[, -5L],
    y = train_data[, 5L],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  model_stack <- caretStack(model_list, method = "knn")

  # Make a confusion matrix
  predictions <- predict(model_stack, newdata = test_data[, -5L])
  classes <- apply(predictions, 1L, function(x) names(x)[which.max(x)])
  classes <- factor(classes, levels = levels(test_data[, 5L]))
  cm <- confusionMatrix(classes, test_data[, 5L])
  expect_is(cm, "confusionMatrix")

  # Check dims
  expect_equal(dim(cm$table), c(3L, 3L))
  # Accuracy should be greater than 0.9
  expect_gt(cm$overall["Accuracy"], 0.9)
})

test_that("Multiclass is not supported for caretEnsemble", {
  data(iris)
  data(models.class)
  data(models.reg)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5L,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5L], 5L)
  )
  model_list <- caretList(
    x = iris[, -5L],
    y = iris[, 5L],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  expect_error(caretEnsemble(model_list))
})

test_that("caretList and caretStack handle imbalanced multiclass data", {
  set.seed(123L)
  n <- 1000L
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- factor(c(rep("A", 700L), rep("B", 200L), rep("C", 100L)))

  my_control <- trainControl(
    method = "cv",
    number = 3L,
    savePredictions = "final",
    classProbs = TRUE
  )

  expect_warning({
    model_list <- caretList(
      x = X,
      y = y,
      trControl = my_control,
      methodList = c("rpart", "glmnet")
    )
  })

  expect_s3_class(model_list, "caretList")
  expect_length(model_list, 2L)

  stack <- caretStack(model_list, method = "glmnet")
  expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = X)
  expect_named(preds, levels(y))
})

test_that("caretList and caretStack handle a large number of classes", {
  set.seed(123L)
  n <- 1000L
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- factor(sample(paste0("Class", 1L:100L), n, replace = TRUE))

  my_control <- trainControl(
    method = "cv",
    number = 2L,
    savePredictions = "final",
    classProbs = TRUE
  )

  expect_warning({
    model_list <- caretList(
      x = X,
      y = y,
      trControl = my_control,
      methodList = "rpart"
    )
  })

  expect_s3_class(model_list, "caretList")

  stack <- caretStack(model_list, method = "rpart")
  expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = X)
  expect_equal(ncol(preds), nlevels(y))
})

test_that("caretList and caretStack handle ordinal multiclass data", {
  data(Boston, package = "MASS")
  Boston$chas <- as.factor(Boston$chas)
  Boston$rad <- factor(paste0("rad_", Boston$rad), ordered = TRUE)

  my_control <- trainControl(
    method = "cv",
    number = 2L,
    savePredictions = "final",
    classProbs = TRUE
  )
  expect_warning({
    model_list <- caretList(
      rad ~ .,
      data = Boston,
      trControl = my_control,
      methodList = c("rpart", "glmnet")
    )
  })

  expect_s3_class(model_list, "caretList")

  stack <- caretStack(model_list, method = "rpart")
  expect_s3_class(stack, "caretStack")

  preds <- predict(stack, newdata = Boston)
  expect_s3_class(preds, "data.table")
  expect_named(preds, levels(Boston$rad))
  expect_equal(rowSums(preds), rep(1.0, nrow(Boston)), tol = 0.0001)
})

test_that("caretList and caretStack produce consistent probability predictions", {
  data(iris)

  my_control <- trainControl(
    method = "cv",
    number = 3L,
    savePredictions = "final",
    classProbs = TRUE
  )

  expect_warning({
    model_list <- caretList(
      x = iris[, -5L],
      y = iris[, 5L],
      trControl = my_control,
      methodList = c("rpart", "glmnet")
    )
  })

  stack <- caretStack(model_list, method = "rpart")

  prob_preds <- predict(stack, newdata = iris[, -5L])
  expect_equal(nrow(prob_preds), nrow(iris))
  expect_equal(ncol(prob_preds), nlevels(iris$Species))
  expect_true(all(rowSums(prob_preds) >= 0.99))
  expect_true(all(rowSums(prob_preds) <= 1.01))
})

test_that("caretList and caretStack handle new levels in prediction data", {
  data(iris)
  idx <- seq_len(nrow(iris))
  idx_train <- sample(idx, 120L)
  idx_test <- setdiff(idx, idx_train)
  train_data <- iris[idx_train, ]
  test_data <- iris[idx_test, ]
  test_data$Species <- factor(as.character(test_data$Species), levels = c(levels(iris$Species), "NewSpecies"))
  test_data$Species[1L] <- "NewSpecies"

  my_control <- trainControl(
    method = "cv",
    number = 2L,
    savePredictions = "final",
    classProbs = TRUE
  )

  expect_warning({
    model_list <- caretList(
      x = train_data[, -5L],
      y = train_data[, 5L],
      trControl = my_control,
      methodList = c("rf", "rpart")
    )
  })

  stack <- caretStack(model_list, method = "rpart")

  preds <- predict(stack, newdata = test_data)
  expect_true(all(levels(preds) %in% levels(train_data$Species)))
})
