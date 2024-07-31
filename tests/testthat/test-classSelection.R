context("Does binary class selection work?")
library(caret)
library(testthat)
library(caretEnsemble)
library(testthat)

# Load and prepare data for subsequent tests
seed <- 2239L
set.seed(seed)
data(models.class)
data(X.class)
data(Y.class)

# Create 80/20 train/test split
index <- createDataPartition(Y.class, p = 0.8)[[1L]]
X.train <- X.class[index, ]
X.test <- X.class[-index, ]
Y.train <- Y.class[index]
Y.test <- Y.class[-index]

#############################################################################
context("Do classifier predictions use the correct target classes?")
#############################################################################

runBinaryLevelValidation <- function(Y.train, Y.test, pos.level = 1L) {
  # Extract levels of response input data
  Y.levels <- levels(Y.train)
  expect_identical(Y.levels, levels(Y.test))
  expect_length(Y.levels, 2L)

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
  unique.levels <- unique(sapply(model.ens$models, function(x) levels(x$pred$obs)[1L]))
  expect_identical(unique.levels, Y.levels[1L])

  # Verify that the training data given to the ensemble model has the
  # same levels in the response as the original, raw data
  expect_identical(levels(model.ens$ens_model$trainingData$.outcome), Y.levels)

  # Create class and probability predictions, as well as class predictions
  # generated from probability predictions using a .5 cutoff
  Y.pred <- predict(model.ens, newdata = X.test, return_class_only = TRUE)
  Y.prob <- predict(model.ens, newdata = X.test, return_class_only = FALSE)
  expect_length(Y.pred, nrow(X.test))
  expect_identical(nrow(Y.prob), nrow(X.test))
  expect_identical(ncol(Y.prob), length(Y.levels))

  Y.cutoff <- factor(
    ifelse(
      Y.prob[[Y.levels[pos.level]]] > 0.5,
      Y.levels[pos.level],
      Y.levels[-pos.level]
    ),
    levels = Y.levels
  )

  # Create confusion matricies for each class prediction vector
  cmat.pred <- confusionMatrix(Y.pred, Y.test, positive = Y.levels[pos.level])
  cmat.cutoff <- confusionMatrix(Y.cutoff, Y.test, positive = Y.levels[pos.level])

  # Verify that the positive level of the Y response is equal to the positive
  # class label used by caret. This could potentially become untrue if
  # the levels of the response were ever rearranged by caretEnsemble at some point.
  expect_identical(cmat.pred$positive, Y.levels[pos.level])

  # Verify that the accuracy score on predicted classes is relatively high. This
  # check exists to avoid previous errors where classifer ensemble predictions were
  # being made using the incorrect level of the response, causing the opposite
  # class labels to be predicted with new data.
  expect_gt(cmat.pred$overall["Accuracy"], 0.79)

  # Similar to the above, ensure that probability predictions are working correctly
  # by checking to see that accuracy is also high for class predictions created
  # from probabilities
  expect_gt(cmat.cutoff$overall["Accuracy"], 0.79)
}

test_that("Ensembled classifiers do not rearrange outcome factor levels", {
  # First run the level selection test using the default levels
  # of the response (i.e. c('No', 'Yes'))
  set.seed(seed)
  runBinaryLevelValidation(Y.train, Y.test, pos.level = 1L)

  # Now reverse the assigment of the response labels as well as
  # the levels of the response factor. Reversing the assignment
  # is necessary to make sure the expected accuracy numbers are
  # the same (i.e. Making a "No" into a "Yes" in the response means
  # predictions of the first class will still be as accurate).
  # Reversing the level order then ensures that the outcome is not
  # releveled at some point by caretEnsemble.
  Y.levels <- levels(Y.train)
  refactor <- function(d) {
    factor(
      ifelse(d == Y.levels[1L], Y.levels[2L], Y.levels[1L]),
      levels = rev(Y.levels)
    )
  }

  set.seed(seed)
  runBinaryLevelValidation(refactor(Y.train), refactor(Y.test))
})

test_that("Target class selection configuration works", {
  # No error
  excluded_class <- validateExcludedClass(1L)
  excluded_class <- validateExcludedClass(2L)

  # Should error
  expect_error(validateExcludedClass("x"), "classification excluded level must be numeric")

  # Check that we can exclude the first class
  Y.levels <- levels(Y.train)
  refactor <- function(d) factor(as.character(d), levels = rev(Y.levels))
  set.seed(seed)
  runBinaryLevelValidation(refactor(Y.train), refactor(Y.test), pos.level = 1L)
})
