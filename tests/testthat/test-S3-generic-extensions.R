# test-S3-generic-extensions

set.seed(107)
suppressMessages({
  library(testthat)
  library(caret)
  library(caretEnsemble)
  library(mlbench)
})

data(models.class)
data(X.class)
data(Y.class)
data(models.reg)
data(X.reg)
data(Y.reg)

data(Sonar)

ctrl1 <- trainControl(
  method = "boot",
  number = 3,
  savePredictions = "final",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE,
  index = createResample(Sonar$Class, 3)
)

ens_ctrl <- trainControl(number = 2)

# a model of class caretList
suppressWarnings({
  model_list1 <- caretList(
    Class ~ .,
    data = Sonar,
    trControl = ctrl1,
    tuneList = list(
      glm = caretModelSpec(method = "glm", family = "binomial"),
      rpart = caretModelSpec(method = "rpart")
    ),
    metric = "ROC"
  )
})

# a model of class train
rfTrain <- train(
  Class ~ .,
  data = Sonar,
  tuneLength = 2,
  metric = "ROC",
  trControl = ctrl1,
  method = "rf"
)

###############################################
context("Ancillary caretList S3 Generic Functions Extensions")
################################################
test_that("c.caretEnsemble can bind two caretList objects", {
  model_list2 <- caretList(
    Class ~ .,
    data = Sonar,
    trControl = ctrl1,
    tuneList = list(
      glm = caretModelSpec(method = "rpart", tuneLength = 2),
      rpart = caretModelSpec(method = "rf", tuneLength = 2)
    ),
    metric = "ROC"
  )

  bigList <- c(model_list1, model_list2)
  ens1 <- caretEnsemble(bigList, trControl = ens_ctrl)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")
  expect_true((length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 4)
})

test_that("c.caretEnsemble can bind a caretList and train object", {
  bigList <- c(model_list1, rfTrain)
  ens1 <- caretEnsemble(bigList, trControl = ens_ctrl)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")
  expect_true((length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 3)
})

test_that("c.caretEnsemble can bind two objects of class train", {
  # a model of class train
  rpartTrain <- train(
    Class ~ .,
    data = Sonar,
    metric = "ROC",
    trControl = ctrl1,
    method = "rpart"
  )

  bigList <- c(rfTrain, rpartTrain)
  ens1 <- caretEnsemble(bigList, trControl = ens_ctrl)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")

  expect_true((length(names(bigList)) == length(unique(names(bigList)))))
  expect_equal(length(unique(names(bigList))), 2)
})

test_that("c.caretList stops for invalid class", {
  expect_error(c.caretList(list()), "class of modelList1 must be 'caretList' or 'train'")
})

test_that("c.train stops for invalid class", {
  expect_error(c.train(list()), "class of modelList1 must be 'caretList' or 'train'")
})

###############################################
context("Edge cases for caretList S3 Generic Functions Extensions")
################################################

test_that("c.caretList combines caretList objects correctly", {
  # Split models.class into two parts
  models_class1 <- models.class[1:2]
  models_class2 <- models.class[3:4]
  class(models_class1) <- class(models_class2) <- "caretList"

  combined_models <- c(models_class1, models_class2)

  expect_s3_class(combined_models, "caretList")
  expect_equal(length(combined_models), length(models.class))
  expect_true(all(names(combined_models) %in% names(models.class)))
})

test_that("c.caretList combines caretList and train objects correctly", {
  models_class1 <- models.class[1:2]
  class(models_class1) <- "caretList"
  single_model <- models.class[[3]]

  combined_models <- c(models_class1, single_model)

  expect_s3_class(combined_models, "caretList")
  expect_equal(length(combined_models), 3)
  expect_true(all(names(combined_models) %in% names(models.class)))
})

test_that("c.train combines train objects correctly", {
  model1 <- models.class[[1]]
  model2 <- models.class[[2]]

  combined_models <- c(model1, model2)

  expect_s3_class(combined_models, "caretList")
  expect_equal(length(combined_models), 2)
  expect_true(all(names(combined_models) %in% names(models.class)[1:2]))
})

test_that("c.caretList handles duplicate names", {
  models_class1 <- models.class[1:2]
  models_class2 <- models.class[1:2]
  class(models_class1) <- class(models_class2) <- "caretList"

  combined_models <- c(models_class1, models_class2)

  expect_s3_class(combined_models, "caretList")
  expect_equal(length(combined_models), 4)
  expect_true(all(make.names(rep(names(models_class1), 2), unique = TRUE) %in% names(combined_models)))
})

test_that("c.caretList and c.train fail for invalid inputs", {
  expect_error(c.caretList(list(a = 1, b = 2)), "class of modelList1 must be 'caretList' or 'train'")
  expect_error(c.train(list(a = 1, b = 2)), "class of modelList1 must be 'caretList' or 'train'")
})

test_that("[.caretList subsets caretList objects correctly", {
  subset_models <- models.class[1:2]

  expect_s3_class(subset_models, "caretList")
  expect_equal(length(subset_models), 2)
  expect_true(all(names(subset_models) %in% names(models.class)[1:2]))
})

test_that("as.caretList.list converts list to caretList", {
  model_list <- list(model1 = models.class[[1]], model2 = models.class[[2]])
  caretlist_object <- as.caretList(model_list)

  expect_s3_class(caretlist_object, "caretList")
  expect_equal(length(caretlist_object), 2)
  expect_true(all(names(caretlist_object) %in% names(model_list)))
})

test_that("as.caretList.list fails for invalid inputs", {
  expect_error(as.caretList(list(a = 1, b = 2)), "object requires all elements of list to be caret models")
})

test_that("predict.caretList works for classification and regression", {
  class_preds <- predict(models.class, newdata = X.class)
  reg_preds <- predict(models.reg, newdata = X.reg)

  expect_true(is.matrix(class_preds))
  expect_true(is.matrix(reg_preds))
  expect_equal(nrow(class_preds), nrow(X.class))
  expect_equal(nrow(reg_preds), nrow(X.reg))
  expect_equal(ncol(class_preds), length(models.class) * 2)
  expect_equal(ncol(reg_preds), length(models.reg))
})

test_that("predict.caretList handles type='prob' for classification", {
  class_probs <- predict(models.class, newdata = X.class)
  expect_true(is.matrix(class_probs))
  expect_equal(nrow(class_probs), nrow(X.class))
  expect_equal(ncol(class_probs), length(models.class) * length(levels(Y.class)))
})
