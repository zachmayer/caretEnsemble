
context("Do the helper functions work?")

test_that("We can multiPredict", {
  data('models_reg')
  out <- multiPredict(models_reg, 'reg', newdata=X.reg)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 3)))
  expect_true(all(colnames(out)==c("rf", "lm", "glm")))
})

context("Does the ensembling and prediction work?")

test_that("We can ensemble regression models", {
  data('models_reg')
  ens.reg <- caretEnsemble(models_reg, optFUN=greedOptRMSE, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg, X.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})

test_that("We can ensemble classification models", {
  data('models_class')
  ens.class <- caretEnsemble(models_class, optFUN=greedOptROC, iter=1000)
  expect_that(ens.class, is_a("caretEnsemble"))
  pred.class <- predict(ens.class, X.class)
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)
})

context("Does the stacking and prediction work?")

test_that("We can ensemble regression models", {

})

test_that("We can ensemble classification models", {

})
