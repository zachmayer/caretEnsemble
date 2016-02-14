context("Does binary class selection work?")
library(caret)
library(caretEnsemble)

data(models.class)
data(X.class)
data(Y.class)

#############################################################################
context("Do classifier predictions use the correct outcome classes?")
#############################################################################

test_that("Ensembled classifiers do not rearrange outcome factor levels", {
  skip_on_cran()
  set.seed(2239)

  # Create 80/20 train/test split on sample data
  index <- createDataPartition(Y.class, p=.8)[[1]]
  X.train <- X.class[index, ]; X.test <- X.class[-index, ]
  Y.train <- Y.class[index]; Y.test <- Y.class[-index]
  lvl <- levels(Y.class)

  # Train a caretEnsemble
  folds <- createFolds(Y.train, k=3, returnTrain=TRUE)
  ctrl <- trainControl(method="cv", number=3, savePredictions="final", classProbs=TRUE, index=folds)
  model.list <- caretList(
    X.train, Y.train, metric = "Kappa",
    trControl = ctrl, methodList = c("glmnet", "rpart"))
  model.ens <- caretEnsemble(model.list)

  # Verify that the observed responses in each fold, for each model,
  # have the same levels and that the first level is equal to the first
  # level in the original data (i.e. Y.class).  This check exists to
  # avoid regressions to bugs like this:
  # https://github.com/zachmayer/caretEnsemble/pull/190
  unique.levels <- unique(sapply(model.ens$models, function(x) levels(x$pred$obs)[1]))
  expect_identical(unique.levels, lvl[1])

  # Create class and probability predictions, as well as class predictions
  # generated from probability predictions using a .5 cutoff
  Y.pred <- predict(model.ens, newdata=X.test, type="raw")
  Y.prob <- predict(model.ens, newdata=X.test, type="prob")
  Y.cutoff <- factor(ifelse(Y.prob > .5, lvl[1], lvl[2]), levels=lvl)

  # Create confusion matricies for each class prediction vector
  cmat.pred <- confusionMatrix(Y.pred, Y.test)
  cmat.cutoff <- confusionMatrix(Y.cutoff, Y.test)

  # Verify that the first level of the Y response is equal to the "positive"
  # class label inferred by caret.  This could potentially become untrue if
  # the levels of the response were ever rearranged by caretEnsemble at some point.
  expect_identical(cmat.pred$positive, lvl[1])

  # Verify that the accuracy score on predicted classes is relatively high.  This
  # check exists to avoid previous errors where classifer ensemble predictions were
  # being made using the incorrect level of the response, causing the opposite
  # class labels to be predicted with new data.
  expect_equal(as.numeric(cmat.pred$overall["Accuracy"]), 0.7931, tol = 0.0001)

  # Similar to the above, ensure that probability predictions are working correctly
  # by checking to see that accuracy is also high for class predictions created
  # from probabilities
  expect_equal(as.numeric(cmat.cutoff$overall["Accuracy"]), 0.7931, tol = 0.0001)

})
