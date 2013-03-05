
#TODO: add tests for every helper function

context("Do the helper functions work?")

test_that("We can make the predobs matrix", {
  data('models_reg')
  out <- makePredObsMatrix(models_reg)
  expect_that(out, is_a("list"))
  expect_true(length(out$obs)==150)
  expect_true(all(dim(out$preds)==c(150, 3)))

})

test_that("We can multiPredict", {
  data('models_reg')
  out <- multiPredict(models_reg, 'reg', newdata=X.reg)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 3)))
  expect_true(all(colnames(out)==c("rf", "lm", "glm")))
})
