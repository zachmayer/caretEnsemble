
context("Does ensembling and prediction work?")

test_that("We can ensemble regression models", {
  load("../../data/models_reg.RData")
  ens.reg <- caretEnsemble(models_reg, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg, X.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})

test_that("We can ensemble classification models", {
  load("../../data/models_class.RData")
  ens.class <- caretEnsemble(models_class, iter=1000)
  expect_that(ens.class, is_a("caretEnsemble"))
  pred.class <- predict(ens.class, X.class)
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)
})
