
context("Do the helper functions work?")

test_that("We can multiPredict", {
  data('models_reg')
  out <- multiPredict(models_reg, 'reg', newdata=X.reg)
  expect_that(out, is_a("matrix"))
  expect_true(all(dim(out)==c(150, 3)))
  expect_true(all(colnames(out)==c("rf", "lm", "glm")))
})
