
context("Does the ensembling work?")

test_that("We can ensemble regression models", {
  data('models_reg')
  ens.reg <- caretEnsemble(models_reg, optFUN=greedOptRMSE, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
})

test_that("We can ensemble classification models", {
  data('models_class')
  ens.class <- caretEnsemble(models_class, optFUN=greedOptROC, iter=1000)
  expect_that(ens.class, is_a("caretEnsemble"))
})

context("Does prediction work?")
