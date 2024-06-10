#############################################################################
context("caretList and caretStack work for multiclass problems")
#############################################################################
test_that("We can predict with caretList and caretStack multiclass problems", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5], 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart")
  )

  p <- predict(model_list, newdata = iris[, -5])
  expect_is(p, "matrix")
  expect_equal(nrow(p), nrow(iris))

  ens <- caretStack(model_list, method = "rpart")

  p_raw <- predict(ens, newdata = iris[, -5], type = "raw")
  expect_is(p_raw, "factor")
  expect_equal(length(p_raw), nrow(iris))

  p <- predict(ens, newdata = iris[, -5], type = "prob")
  expect_is(p, "data.frame")
  expect_equal(nrow(p), nrow(iris))
})

test_that("Columns for caretList predictions are correct and ordered", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5], 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  num_methods <- length(model_list)
  num_classes <- length(unique(iris$Species))

  # Check the number of rows and columns is correct
  p <- predict(model_list, newdata = iris[, -5])
  expect_equal(dim(p), c(nrow(iris), num_methods * num_classes))

  methods <- names(model_list)
  classes <- levels(iris$Species)
  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1, function(x) paste(x[2], x[1], sep = "_"))

  # Check the names of the columns are correct
  expect_true(all(colnames(p) %in% ordered_colnames))

  # Check that the columns are ordered correctly
  expect_equal(colnames(p), ordered_colnames)
})

test_that("Columns for caretStack are correct", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5], 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  model_stack <- caretStack(model_list, method = "knn")

  num_classes <- length(unique(iris$Species))

  # Check the number of rows and columns is correct
  p_raw <- predict(model_stack, newdata = iris[, -5], type = "raw")
  expect_equal(length(p_raw), nrow(iris))
  p_prob <- predict(model_stack, newdata = iris[, -5], type = "prob")
  expect_equal(dim(p_prob), c(nrow(iris), num_classes))

  classes <- levels(iris$Species)

  # Check that the columns are ordered correctly
  expect_equal(colnames(p_prob), classes)
})

test_that("Underscores are supported in method and class names in caretList and caretStack", {
  data(iris)
  # Rename values and levels to have underscores
  levels(iris[, 5]) <- c("setosa_1", "versicolor_2", "virginica_3")
  iris[, 5] <- factor(iris[, 5])

  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5], 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet_1 = caretModelSpec(method = "nnet", tuneGrid = expand.grid(.size = c(1, 3, 5), .decay = 0.3), trace = FALSE),
      nnet_2 = caretModelSpec(method = "nnet", tuneGrid = expand.grid(.size = 3, .decay = c(0.1, 0.3, 0.5)), trace = FALSE)
    )
  )

  methods <- names(model_list)
  classes <- levels(iris[, 5])

  p <- predict(model_list, newdata = iris[, -5])

  class_method_combinations <- expand.grid(classes, methods)
  ordered_colnames <- apply(class_method_combinations, 1, function(x) paste(x[2], x[1], sep = "_"))
  expect_equal(colnames(p), ordered_colnames)

  model_stack <- caretStack(model_list, method = "knn")
  p_prob <- predict(model_stack, newdata = iris[, -5], type = "prob")
  expect_equal(colnames(p_prob), classes)
  p_raw <- predict(model_stack, newdata = iris[, -5], type = "raw")
  expect_equal(levels(p_raw), classes)
})

test_that("We can make a confusion matrix", {
  data(iris)

  set.seed(42)
  n <- nrow(iris)
  train_indices <- sample(seq_len(n), n * 0.8)
  train_data <- iris[train_indices, ]
  test_data <- iris[-train_indices, ]

  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(train_data[, 5], 5)
  )
  model_list <- caretList(
    x = train_data[, -5],
    y = train_data[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  model_stack <- caretStack(model_list, method = "knn")

  # Make a confusion matrix
  predictions <- predict(model_stack, newdata = test_data[, -5], type = "raw")
  cm <- confusionMatrix(predictions, test_data[, 5])
  expect_is(cm, "confusionMatrix")

  # Check dims
  expect_true(all(dim(cm$table) == c(3, 3)))
  # Accuracy should be greater than 0.9
  expect_true(cm$overall["Accuracy"] > 0.9)
})

test_that("Multiclass is not supported for caretEnsemble", {
  data(iris)
  data(models.class)
  data(models.reg)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris[, 5], 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart"),
    tuneList = list(
      nnet = caretModelSpec(method = "nnet", trace = FALSE)
    )
  )

  expect_error(caretEnsemble(model_list))
})
