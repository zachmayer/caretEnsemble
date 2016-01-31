#NOTE: I don't see how this tests the parallel code.  It seems to just test caretEnsemble

context("Parallelization works")
test_that("predict.caretEnsemble works in parallel", {
  skip_on_cran()
  X_reg <- model.matrix(~ ., iris[, -1])
  X_reg_big <- do.call(rbind, lapply(1:100, function(x) X_reg))
  Y_reg <- iris[, 1]
  expect_warning(ens_reg <- caretEnsemble(caretList(X_reg, Y_reg, methodList=c("lm", "glm"))))

  #Basic
  expect_warning(pred_reg <- predict(ens_reg, newdata = X_reg))
  expect_warning(pred_reg2 <- predict(ens_reg, newdata = X_reg_big))
  expect_equal(pred_reg, pred_reg2[1:length(pred_reg)])

  #Return se
  expect_warning(pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE))
  expect_warning(pred_reg2 <- predict(ens_reg, newdata = X_reg_big, se = TRUE))
  row.names(pred_reg) <- NULL
  row.names(pred_reg2) <- NULL
  expect_equal(pred_reg, pred_reg2[1:nrow(pred_reg), ])

  #Return weights
  expect_warning(pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE, return_weights = TRUE))
  expect_warning(
    pred_reg2 <- predict(
      ens_reg, newdata = X_reg_big, se = TRUE, return_weights = TRUE
    )
  )
  row.names(pred_reg) <- NULL
  row.names(pred_reg2) <- NULL
  expect_equal(pred_reg$fit, pred_reg2$fit[1:length(pred_reg$fit)])
})
