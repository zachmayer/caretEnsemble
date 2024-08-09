# Load and prepare data for subsequent tests
seed <- 2239L
set.seed(seed)
utils::data(iris)

# Create 80/20 train/test split
target_col <- which(names(iris) == "Species")
index <- caret::createDataPartition(iris[, target_col], p = 0.8)[[1L]]
X.train <- iris[index, -target_col]
X.test <- iris[-index, -target_col]
Y.train <- iris[index, target_col]
Y.test <- iris[-index, target_col]

runBinaryLevelValidation <- function(Y.train, Y.test, pos.level = 1L) {
  # Extract levels of response input data
  Y.levels <- levels(Y.train)
  testthat::expect_identical(Y.levels, levels(Y.test))
  testthat::expect_length(Y.levels, 3L)

  # Train a caret ensemble
  model.list <- caretList(
    X.train, Y.train,
    methodList = c("rpart", "glmnet")
  )
  model.ens <- caretEnsemble(model.list)

  # Verify that the observed responses in each fold, for each model,
  # have the same levels and that the first level is equal to the first
  # level in the original data (i.e. Y.class). This check exists to
  # avoid regressions to bugs like this:
  # https://github.com/zachmayer/caretEnsemble/pull/190
  unique.levels <- unique(vapply(model.ens$models, function(x) levels(x$pred$obs)[1L], character(1L)))
  testthat::expect_identical(unique.levels, Y.levels[1L])

  # Verify that the training data given to the ensemble model has the
  # same levels in the response as the original, raw data
  testthat::expect_identical(levels(model.ens$ens_model$trainingData$.outcome), Y.levels)

  # Create class and probability predictions, as well as class predictions
  # generated from probability predictions using a .5 cutoff
  Y.pred <- predict(model.ens, newdata = X.test, return_class_only = TRUE)
  Y.prob <- predict(model.ens, newdata = X.test, return_class_only = FALSE)
  testthat::expect_length(Y.pred, nrow(X.test))
  testthat::expect_identical(nrow(Y.prob), nrow(X.test))
  testthat::expect_identical(ncol(Y.prob), length(Y.levels))

  Y.cutoff <- factor(
    ifelse(
      Y.prob[[Y.levels[pos.level]]] > 0.5,
      Y.levels[pos.level],
      Y.levels[-pos.level]
    ),
    levels = Y.levels
  )

  # Create confusion matricies for each class prediction vector
  cmat.pred <- caret::confusionMatrix(Y.pred, Y.test, positive = Y.levels[pos.level])
  cmat.cutoff <- caret::confusionMatrix(Y.cutoff, Y.test, positive = Y.levels[pos.level])

  # Verify that the positive level of the Y response is equal to the positive
  # class label used by caret. This could potentially become untrue if
  # the levels of the response were ever rearranged by caretEnsemble at some point.
  testthat::expect_identical(cmat.pred$positive, Y.levels[pos.level])

  # Verify that the accuracy score on predicted classes is relatively high. This
  # check exists to avoid previous errors where classifer ensemble predictions were
  # being made using the incorrect level of the response, causing the opposite
  # class labels to be predicted with new data.
  testthat::expect_gt(cmat.pred$overall["Accuracy"], 0.60)

  # Similar to the above, ensure that probability predictions are working correctly
  # by checking to see that accuracy is also high for class predictions created
  # from probabilities
  testthat::expect_gt(cmat.cutoff$overall["Accuracy"], 0.60)
}

#############################################################################
testthat::context("Do classifier predictions use the correct target classes?")
#############################################################################

testthat::test_that("validateExcludedClass for multiclass", {
  for (excluded_class in c(1L, 2L, 3L)) {
    testthat::expect_silent(validateExcludedClass(excluded_class))
  }
  testthat::expect_error(validateExcludedClass("x"), "classification excluded level must be numeric")
})

testthat::test_that("Target class selection configuration works", {
  Y.levels <- levels(Y.train)
  refactor <- function(d) factor(as.character(d), levels = rev(Y.levels))
  set.seed(seed)
  for (pos in c(1L, 2L, 3L)) {
    runBinaryLevelValidation(Y.train, Y.test, pos.level = pos)
    runBinaryLevelValidation(refactor(Y.train), refactor(Y.test), pos.level = pos)
  }
})
