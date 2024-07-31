# test-S3-generic-extensions
set.seed(107L)
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

# A common control to use for both test fixtures
my_control <- trainControl(
  method = "cv",
  number = 2L,
  savePredictions = "final",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE,
  index = createResample(Sonar$Class, 3L)
)

# a model of class caretList
model_list1 <- caretList(
  Class ~ .,
  data = Sonar,
  trControl = my_control,
  tuneList = list(
    glm = caretModelSpec(method = "rf"),
    rpart = caretModelSpec(method = "rpart")
  )
)

# a model of class train
rpart_model <- train(
  Class ~ .,
  data = Sonar,
  tuneLength = 2L,
  metric = "ROC",
  trControl = my_control,
  method = "rpart"
)

###############################################
context("Ancillary caretList S3 Generic Functions Extensions")
################################################
test_that("c.caretEnsemble can bind two caretList objects", {
  model_list2 <- caretList(
    Class ~ .,
    data = Sonar,
    trControl = my_control,
    tuneList = list(
      glm = caretModelSpec(method = "rpart", tuneLength = 2L),
      rpart = caretModelSpec(method = "rf", tuneLength = 2L)
    )
  )

  bigList <- c(model_list1, model_list2)
  ens1 <- caretEnsemble(bigList)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")
  expect_identical(anyDuplicated(names(bigList)), 0L)
  expect_length(unique(names(bigList)), 4L)
})

test_that("c.caretEnsemble can bind a caretList and train object", {
  bigList <- c(model_list1, rpart_model)
  ens1 <- caretEnsemble(bigList)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")
  expect_identical(anyDuplicated(names(bigList)), 0L)
  expect_length(unique(names(bigList)), 3L)
})

test_that("c.caretList can bind two objects of class train", {
  bigList <- c(rpart_model, rpart_model)
  ens1 <- caretEnsemble(bigList)

  expect_is(bigList, "caretList")
  expect_is(ens1, "caretEnsemble")

  expect_identical(anyDuplicated(names(bigList)), 0L)
  expect_length(unique(names(bigList)), 2L)
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
  models_class1 <- models.class[1L:2L]
  models_class2 <- models.class[3L:4L]
  class(models_class1) <- class(models_class2) <- "caretList"

  combined_models <- c(models_class1, models_class2)

  expect_s3_class(combined_models, "caretList")
  expect_length(combined_models, length(models.class))
  expect_true(all(names(combined_models) %in% names(models.class)))
})

test_that("c.caretList combines caretList and train objects correctly", {
  models_class1 <- models.class[1L:2L]
  class(models_class1) <- "caretList"
  single_model <- models.class[[3L]]

  combined_models <- c(models_class1, single_model)

  expect_s3_class(combined_models, "caretList")
  expect_length(combined_models, 3L)
  expect_true(all(names(combined_models) %in% names(models.class)))
})

test_that("c.train combines train objects correctly", {
  model1 <- models.class[[1L]]
  model2 <- models.class[[2L]]

  combined_models <- c(model1, model2)

  expect_s3_class(combined_models, "caretList")
  expect_length(combined_models, 2L)
  expect_true(all(names(combined_models) %in% names(models.class)[1L:2L]))
})

test_that("c.caretList handles duplicate names", {
  models_class1 <- models.class[1L:2L]
  models_class2 <- models.class[1L:2L]
  class(models_class1) <- class(models_class2) <- "caretList"

  combined_models <- c(models_class1, models_class2)

  expect_s3_class(combined_models, "caretList")
  expect_length(combined_models, 4L)
  expect_true(all(make.names(rep(names(models_class1), 2L), unique = TRUE) %in% names(combined_models)))
})

test_that("c.caretList and c.train fail for invalid inputs", {
  expect_error(c.caretList(list(a = 1L, b = 2L)), "class of modelList1 must be 'caretList' or 'train'")
  expect_error(c.train(list(a = 1L, b = 2L)), "class of modelList1 must be 'caretList' or 'train'")
})

test_that("[.caretList subsets caretList objects correctly", {
  subset_models <- models.class[1L:2L]

  expect_s3_class(subset_models, "caretList")
  expect_length(subset_models, 2L)
  expect_true(all(names(subset_models) %in% names(models.class)[1L:2L]))
})

test_that("as.caretList.list converts list to caretList", {
  model_list <- list(model1 = models.class[[1L]], model2 = models.class[[2L]])
  caretlist_object <- as.caretList(model_list)

  expect_s3_class(caretlist_object, "caretList")
  expect_length(caretlist_object, 2L)
  expect_true(all(names(caretlist_object) %in% names(model_list)))
})

test_that("as.caretList.list fails for invalid inputs", {
  expect_error(as.caretList(list(a = 1L, b = 2L)), "object requires all elements of list to be caret models")
})

test_that("as.caretList.list names lists without names", {
  models.no.name <- models.class
  names(models.no.name) <- NULL
  class(models.no.name) <- "list"
  expect_null(names(models.no.name))
  cl <- as.caretList(models.no.name)
  expect_named(cl, unname(sapply(models.class, "[[", "method")))
})
test_that("as.caretList fails on non-list", {
  expect_error(as.caretList(1L), "object must be a list")
})

test_that("predict.caretList works for classification and regression", {
  class_preds <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  reg_preds <- predict(models.reg, newdata = X.reg)

  expect_is(class_preds, "data.table")
  expect_is(reg_preds, "data.table")
  expect_equal(nrow(class_preds), nrow(X.class))
  expect_equal(nrow(reg_preds), nrow(X.reg))
  expect_equal(ncol(class_preds), length(models.class) * 2L)
  expect_equal(ncol(reg_preds), length(models.reg))
})

test_that("predict.caretList handles type='prob' for classification", {
  class_probs <- predict(models.class, newdata = X.class, excluded_class_id = 0L)
  expect_is(class_probs, "data.table")
  expect_equal(nrow(class_probs), nrow(X.class))
  expect_equal(ncol(class_probs), length(models.class) * nlevels(Y.class))
})
