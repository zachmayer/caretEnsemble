context("Parallelization works")
test_that("predict.caretEnsemble works in parallel", {
  skip_on_cran()
  X_reg <- model.matrix(~ ., iris[, -1])
  X_reg_big <- do.call(rbind, lapply(1:100, function(x) X_reg))
  Y_reg <- iris[, 1]
  ens_reg <- caretEnsemble(caretList(X_reg, Y_reg, methodList=c("lm", "glm")))

  #Basic
  pred_reg <- predict(ens_reg, newdata = X_reg)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big)
  expect_equal(pred_reg, pred_reg2[1:length(pred_reg)])

  #Don't keep NAs
  pred_reg <- predict(ens_reg, newdata = X_reg, keepNA = FALSE)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big, keepNA = FALSE)
  expect_equal(pred_reg, pred_reg2[1:length(pred_reg)])

  #Return se
  pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big, se = TRUE)
  expect_equal(pred_reg, pred_reg2[1:nrow(pred_reg), ])

  #Return weights
  pred_reg <- predict(ens_reg, newdata = X_reg, se = TRUE, return_weights = TRUE)
  pred_reg2 <- predict(
    ens_reg, newdata = X_reg_big, se = TRUE, return_weights = TRUE
    )
  expect_equal(pred_reg$fit, pred_reg2$fit[1:length(pred_reg$fit)])

  #Don't keep NAs, return se
  pred_reg <- predict(ens_reg, newdata = X_reg, keepNA = FALSE, se = TRUE)
  pred_reg2 <- predict(ens_reg, newdata = X_reg_big, keepNA = FALSE, se = TRUE)
  expect_equal(pred_reg, pred_reg2[1:nrow(pred_reg), ])

  #Don't keep NAs, return se, return weights
  pred_reg <- predict(
    ens_reg, newdata = X_reg, keepNA = FALSE, se = TRUE, return_weights = TRUE)
  pred_reg2 <- predict(
    ens_reg, newdata = X_reg_big, keepNA = FALSE, se = TRUE,
    return_weights = TRUE)
  expect_equal(pred_reg$fit, pred_reg2$fit[1:nrow(pred_reg)])
})
