context("Does binary class selection work?")
library(caret)
library(testthat)
library(caretEnsemble)

# Load and prepare data for subsequent tests
seed <- 2239
set.seed(seed)
data(models.class)
data(X.class)
data(Y.class)

# Create 80/20 train/test split
index <- createDataPartition(Y.class, p = .8)[[1]]
X.train <- X.class[index, ]
X.test <- X.class[-index, ]
Y.train <- Y.class[index]
Y.test <- Y.class[-index]

#############################################################################
context("Do classifier predictions use the correct target classes?")
#############################################################################

runBinaryLevelValidation <- function(Y.train, Y.test, pos.level = 1) {
  # Extract levels of response input data
  Y.levels <- levels(Y.train)
  expect_identical(Y.levels, levels(Y.test))
  expect_equal(length(Y.levels), 2)

  # Manually generate fold indexes.  Note that this must be
  # done explicitly because using built-in caret functions like
  # "createFolds" generate splits that are dependent upon the alphabetical
  # order of the factor levels in the response (which is what this test module
  # needs to prove invariance to).  This happens because createFolds uses
  # the base table function to create class frequency counts and that table
  # command sorts results alphabetically.
  k <- 3
  folds <- sample(seq_along(Y.train))
  folds <- setNames(split(folds, seq_along(folds) %% k), sprintf("Fold%s", 1:k))
  folds <- lapply(folds, function(x) setdiff(seq_along(Y.train), x))
  fold.idx <- sort(unique(unlist(folds)))
  expect_true(all(fold.idx == seq_along(Y.train)), "CV indexes not generated correctly")

  # Train a caret ensemble
  ctrl <- trainControl(method = "cv", savePredictions = "final", classProbs = TRUE, index = folds)
  model.list <- caretList(
    X.train, Y.train,
    metric = "Accuracy",
    trControl = ctrl, methodList = c("rpart", "glmnet")
  )
  model.ens <- caretEnsemble(model.list)

  # Verify that the observed responses in each fold, for each model,
  # have the same levels and that the first level is equal to the first
  # level in the original data (i.e. Y.class).  This check exists to
  # avoid regressions to bugs like this:
  # https://github.com/zachmayer/caretEnsemble/pull/190
  unique.levels <- unique(sapply(model.ens$models, function(x) levels(x$pred$obs)[1]))
  expect_identical(unique.levels, Y.levels[1])

  # Verify that the training data given to the ensemble model has the
  # same levels in the response as the original, raw data
  expect_identical(levels(model.ens$ens_model$trainingData$.outcome), Y.levels)

  # Create class and probability predictions, as well as class predictions
  # generated from probability predictions using a .5 cutoff
  Y.pred <- predict(model.ens, newdata = X.test, type = "raw")
  Y.prob <- predict(model.ens, newdata = X.test, type = "prob")
  Y.cutoff <- factor(ifelse(Y.prob > .5, Y.levels[pos.level], Y.levels[-pos.level]), levels = Y.levels)

  # Create confusion matricies for each class prediction vector
  cmat.pred <- confusionMatrix(Y.pred, Y.test, positive = Y.levels[pos.level])
  cmat.cutoff <- confusionMatrix(Y.cutoff, Y.test, positive = Y.levels[pos.level])

  # Verify that the positive level of the Y response is equal to the positive
  # class label used by caret.  This could potentially become untrue if
  # the levels of the response were ever rearranged by caretEnsemble at some point.
  expect_identical(cmat.pred$positive, Y.levels[pos.level])

  # Verify that the accuracy score on predicted classes is relatively high.  This
  # check exists to avoid previous errors where classifer ensemble predictions were
  # being made using the incorrect level of the response, causing the opposite
  # class labels to be predicted with new data.
  expect_equal(as.numeric(cmat.pred$overall["Accuracy"]), 0.862, tol = 0.1)

  # Similar to the above, ensure that probability predictions are working correctly
  # by checking to see that accuracy is also high for class predictions created
  # from probabilities
  expect_equal(as.numeric(cmat.cutoff$overall["Accuracy"]), 0.862, tol = 0.1)
}

test_that("Ensembled classifiers do not rearrange outcome factor levels", {
  skip_on_cran()

  # Make sure that caretEnsemble uses the first level in the
  # outcome factor as the target class
  bin.level <- getBinaryTargetLevel()
  setBinaryTargetLevel(1L)

  # First run the level selection test using the default levels
  # of the response (i.e. c('No', 'Yes'))
  set.seed(seed)
  runBinaryLevelValidation(Y.train, Y.test)

  # Now reverse the assigment of the response labels as well as
  # the levels of the response factor.  Reversing the assignment
  # is necessary to make sure the expected accuracy numbers are
  # the same (i.e. Making a "No" into a "Yes" in the response means
  # predictions of the first class will still be as accurate).
  # Reversing the level order then ensures that the outcome is not
  # releveled at some point by caretEnsemble.
  Y.levels <- levels(Y.train)
  refactor <- function(d) {
    factor(
      ifelse(d == Y.levels[1], Y.levels[2], Y.levels[1]),
      levels = rev(Y.levels)
    )
  }

  set.seed(seed)
  runBinaryLevelValidation(refactor(Y.train), refactor(Y.test))

  # Set the target binary level back to what it was before this test
  setBinaryTargetLevel(bin.level)
})

test_that("Target class selection configuration works", {
  skip_on_cran()

  # Get the current target binary level
  bin.level <- getBinaryTargetLevel()

  # Verify binary target level argument validation
  expect_error(setBinaryTargetLevel("x"))

  # Configure caret ensemble to use the second class as the target
  setBinaryTargetLevel(2L)

  Y.levels <- levels(Y.train)
  refactor <- function(d) factor(as.character(d), levels = rev(Y.levels))

  set.seed(seed)
  runBinaryLevelValidation(refactor(Y.train), refactor(Y.test), pos.level = 2)

  # Set the target binary level back to what it was before this test
  setBinaryTargetLevel(bin.level)
})
