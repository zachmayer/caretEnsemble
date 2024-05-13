#############################################################################
context("caretList and caretStack work for multiclass problems")
#############################################################################
test_that("Multiclass caretList and caretStack", {
  data(iris)
  my_control <- caret::trainControl(
    method = "boot",
    number = 5,
    savePredictions = "final",
    classProbs = TRUE,
    index = caret::createResample(iris$Species, 5)
  )
  model_list <- caretList(
    x = iris[, -5],
    y = iris[, 5],
    trControl = my_control,
    methodList = c("glmnet", "rpart")
  )
  ens <- caretStack(model_list, method = "rpart")

  p_raw <- predict(ens, iris[, -5], type = "raw")
  expect_is(p_raw, "factor")
  expect_equal(length(p_raw), nrow(iris))

  p <- predict(ens, iris[, -5], type = "prob")
  expect_is(p, "numeric")
  expect_equal(length(p), nrow(iris))
})
