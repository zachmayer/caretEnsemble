context("Parallelization works")
test_that("predict.caretEnsemble works in parallel", {
  skip_on_cran()
  X.reg <- model.matrix(~ ., iris[, -1])
  X.reg.big <- do.call(rbind, lapply(1:100, function(x) X.reg))
  Y.reg <- iris[, 1]
  ens.reg <- caretEnsemble(caretList(X.reg, Y.reg, methodList=c("lm", "glm")))

  #Basic
  pred.reg <- predict(ens.reg, newdata = X.reg)
  pred.reg2 <- predict(ens.reg, newdata = X.reg.big)
  expect_equal(pred.reg, pred.reg2[1:length(pred.reg)])

  #Don't keep NAs
  pred.reg <- predict(ens.reg, newdata = X.reg, keepNA = FALSE)
  pred.reg2 <- predict(ens.reg, newdata = X.reg.big, keepNA = FALSE)
  expect_equal(pred.reg, pred.reg2[1:length(pred.reg)])

  #Return se
  pred.reg <- predict(ens.reg, newdata = X.reg, se = TRUE)
  pred.reg2 <- predict(ens.reg, newdata = X.reg.big, se = TRUE)
  expect_equal(pred.reg, pred.reg2[1:nrow(pred.reg), ])

  #Return weights
  pred.reg <- predict(ens.reg, newdata = X.reg, se = TRUE, return_weights = TRUE)
  pred.reg2 <- predict(
    ens.reg, newdata = X.reg.big, se = TRUE, return_weights = TRUE
    )
  expect_equal(pred.reg$preds, pred.reg2$preds[1:nrow(pred.reg$preds),])

  #Don't keep NAs, return se
  pred.reg <- predict(ens.reg, newdata = X.reg, keepNA = FALSE, se = TRUE)
  pred.reg2 <- predict(ens.reg, newdata = X.reg.big, keepNA = FALSE, se = TRUE)
  expect_equal(pred.reg, pred.reg2[1:nrow(pred.reg), ])

  #Don't keep NAs, return se, return weights
  pred.reg <- predict(
    ens.reg, newdata = X.reg, keepNA = FALSE, se = TRUE, return_weights = TRUE)
  pred.reg2 <- predict(
    ens.reg, newdata = X.reg.big, keepNA = FALSE, se = TRUE,
    return_weights = TRUE)
  expect_equal(pred.reg$preds, pred.reg2$preds[1:nrow(pred.reg$preds),])
})
