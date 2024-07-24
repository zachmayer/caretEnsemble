# NOTE: I don't see how this tests the parallel code.  It seems to just test caretEnsemble

context("Parallelization works")
test_that("predict.caretEnsemble works in parallel", {
  skip_on_cran()
  X_reg <- model.matrix(~., iris[, -1L])
  X_reg_big <- do.call(rbind, lapply(1L:100L, function(x) X_reg))
  Y_reg <- iris[, 1L]
  suppressWarnings(ens_reg <- caretEnsemble(caretList(X_reg, Y_reg, methodList = c("lm", "glm"))))

  # Basic
  suppressWarnings(pred_reg <- predict(ens_reg, newdata = X_reg))
  suppressWarnings(pred_reg2 <- predict(ens_reg, newdata = X_reg_big))
  pred_reg2 <- pred_reg2[seq_along(pred_reg)]

  pred_reg <- unname(pred_reg)
  pred_reg2 <- unname(pred_reg2)
  expect_equal(pred_reg, pred_reg2)

  # Return se
  suppressWarnings(pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE))
  suppressWarnings(pred_reg2 <- predict(ens_reg, newdata = X_reg_big, se = TRUE))
  row.names(pred_reg) <- NULL
  row.names(pred_reg2) <- NULL
  expect_equal(pred_reg, pred_reg2[seq_len(nrow(pred_reg)), ])

  # Return weights
  suppressWarnings(pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE, return_weights = TRUE))
  suppressWarnings(
    pred_reg2 <- predict(
      ens_reg,
      newdata = X_reg_big, se = TRUE, return_weights = TRUE
    )
  )
  row.names(pred_reg) <- NULL
  row.names(pred_reg2) <- NULL
  expect_equal(pred_reg$fit, pred_reg2$fit[seq_along(pred_reg$fit)])
})
