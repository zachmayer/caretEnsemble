# NOTE: I don't see how this tests the parallel code. It seems to just test caretEnsemble
data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

context("Parallelization works")
test_that("predict.caretEnsemble works in parallel", {
  X_reg_big <- data.table::rbindlist(lapply(1L:100L, function(x) data.table(X.reg)))
  ens_reg <- caretEnsemble(models.reg)

  # Basic
  pred_reg <- predict(ens_reg, newdata = X.reg)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big)
  pred_reg2 <- pred_reg2[seq_len(nrow(pred_reg))]
  expect_equivalent(pred_reg, pred_reg2)

  # Return se
  pred_reg <- predict(ens_reg, newdata = X.reg, se = TRUE)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big, se = TRUE)
  pred_reg2 <- pred_reg2[seq_len(nrow(pred_reg))]
  expect_equivalent(pred_reg, pred_reg2)

  # Return weights
  pred_reg <- predict(ens_reg, newdata = X.reg, se = TRUE, return_weights = TRUE)
  pred_reg2 <- predict(
    ens_reg,
    newdata = X_reg_big, se = TRUE, return_weights = TRUE
  )
  pred_reg2 <- pred_reg2[seq_len(nrow(pred_reg))]
  expect_equivalent(pred_reg, pred_reg2)
})
