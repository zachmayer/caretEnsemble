# Helper function to create a simple dataset
create_dataset <- function(n_samples, n_features, n_targets, coef = NULL, noise = 0L) {
  X <- matrix(stats::runif(n_samples * n_features), nrow = n_samples)

  if (is.null(coef)) {
    coef <- matrix(stats::runif(n_features * n_targets), nrow = n_features)
  } else if (is.vector(coef)) {
    coef <- matrix(coef, ncol = 1L)
  }

  # Normalize coefficients
  coef <- apply(coef, 2L, function(col) col / sum(abs(col)))

  Y <- X %*% coef + matrix(stats::rnorm(n_samples * n_targets, 0.0, noise), nrow = n_samples)

  if (n_targets > 1L) {
    # Ensure all values are positive
    Y <- Y - min(Y) + 1e-6
    # Normalize rows to sum to 1
    Y <- Y / rowSums(Y)
  }

  list(X = X, Y = Y, coef = coef)
}

# Create datasets for reuse
set.seed(42L)
regression_data <- create_dataset(100L, 5L, 1L, noise = 0.1)
multi_regression_data <- create_dataset(100L, 5L, 3L)

Y_binary <- as.integer(regression_data$Y > mean(regression_data$Y))
Y_multi_binary <- as.integer(multi_regression_data$Y > mean(multi_regression_data$Y))

# Test for regression (one col)
testthat::test_that("greedyMSE works for regression", {
  model <- greedyMSE(regression_data$X, regression_data$Y)
  testthat::expect_lt(model$RMSE, stats::sd(regression_data$Y)) # Model should be better than baseline
  # High correlation with true values
  testthat::expect_gt(stats::cor(predict(model, regression_data$X), regression_data$Y), 0.8)
})

# Test for binary classification (one col)
testthat::test_that("greedyMSE works for binary classification", {
  model <- greedyMSE(regression_data$X, Y_binary)
  predictions <- predict(model, regression_data$X)
  accuracy <- mean((predictions > 0.5) == Y_binary)
  testthat::expect_gt(accuracy, 0.7) # Accuracy should be better than random guessing
})

# Test for multiple regression (many cols)
testthat::test_that("greedyMSE works for multiple regression", {
  model <- greedyMSE(multi_regression_data$X, multi_regression_data$Y)

  testthat::expect_lt(model$RMSE, 0.3)
  predictions <- predict(model, multi_regression_data$X)
  correlations <- diag(stats::cor(predictions, multi_regression_data$Y))
  testthat::expect_gt(min(correlations), 0.7) # High correlation for all targets
})

# Test for multiclass classification (many cols)
testthat::test_that("greedyMSE works for multiclass classification", {
  model <- greedyMSE(multi_regression_data$X, Y_multi_binary)
  predictions <- predict(model, multi_regression_data$X)
  accuracy <- mean(apply(predictions, 1L, which.max) == apply(multi_regression_data$Y, 1L, which.max))
  testthat::expect_gt(accuracy, 0.6) # Accuracy should be better than random guessing
})

# Edge cases
testthat::test_that("greedyMSE handles edge cases", {
  # 1. Single feature
  data <- create_dataset(100L, 1L, 1L)
  testthat::expect_equal(greedyMSE(data$X, data$Y)$RMSE, 0.0, tolerance = 1e-6)

  # 2. Perfect multicollinearity
  X <- matrix(1L, nrow = 100L, ncol = 2L)
  Y <- X[, 1L] + stats::rnorm(100L, 0.0, 0.1)
  testthat::expect_equal(greedyMSE(data$X, data$Y)$RMSE, 0.0, tolerance = 1e-6)

  # 3. All zero features
  X <- matrix(0L, nrow = 100L, ncol = 5L)
  Y <- matrix(stats::rnorm(100L), ncol = 1L)
  model <- greedyMSE(X, Y)
  testthat::expect_equal(model$RMSE, stats::sd(Y), tolerance = 1e-2)

  # 4. Constant target
  X <- matrix(stats::runif(500L), nrow = 100L)
  Y <- matrix(rep(1L, 100L), ncol = 1L)
  model <- greedyMSE(X, Y)
  testthat::expect_equal(model$RMSE, 0.5, tolerance = 0.1)

  # 5. Very large values
  X <- matrix(stats::runif(500L, 1.0e6, 1.0e7), nrow = 100L)
  Y <- matrix(rowSums(X) + stats::rnorm(100L, 0L, 1.0e5), ncol = 1L)
  model <- greedyMSE(X, Y)
  pred <- predict(model, X)
  testthat::expect_gt(cor(pred, Y), 0.5)
})

# Regression ensembling test
testthat::test_that("greedyMSE can be used for regression ensembling with GLM, rpart, and RF", {
  X <- regression_data$X
  Y <- regression_data$Y

  # GLM
  glm_model <- stats::glm(Y ~ X, family = stats::gaussian())
  glm_pred <- stats::predict(glm_model, newdata = data.frame(X))

  # rpart
  rpart_model <- rpart::rpart(Y ~ X)
  rpart_pred <- stats::predict(rpart_model, newdata = data.frame(X))

  # greedyMSE
  greedy_model <- greedyMSE(X, Y)
  greedy_pred <- predict(greedy_model, X)

  # Ensemble
  ensemble_X <- cbind(glm_pred, rpart_pred, greedy_pred)
  ensemble_model <- greedyMSE(ensemble_X, Y)

  # Check if ensemble performs better than the best individual model
  individual_rmse <- c(
    sqrt(mean((Y - glm_pred)^2L)),
    sqrt(mean((Y - rpart_pred)^2L)),
    greedy_model$RMSE
  )
  testthat::expect_lte(ensemble_model$RMSE, min(individual_rmse))
})

# Classification ensembling test
testthat::test_that("greedyMSE can be used for classification ensembling with GLM, rpart, and RF", {
  X <- regression_data$X
  Y_binary <- as.integer(regression_data$Y > stats::median(regression_data$Y))

  # GLM (logistic regression)
  glm_model <- stats::glm(Y_binary ~ X, family = stats::binomial())
  glm_pred <- stats::predict(glm_model, newdata = data.frame(X), type = "response")

  # rpart
  rpart_model <- rpart::rpart(Y_binary ~ X, method = "class")
  rpart_pred <- stats::predict(rpart_model, newdata = data.frame(X), type = "prob")[, 2L]

  # Random Forest
  rf_model <- randomForest::randomForest(X, as.factor(Y_binary))
  rf_pred <- stats::predict(rf_model, newdata = X, type = "prob")[, 2L]

  # greedyMSE
  greedy_model <- greedyMSE(X, matrix(Y_binary, ncol = 1L))
  greedy_pred <- predict(greedy_model, X)

  # Ensemble
  ensemble_X <- cbind(glm_pred, rpart_pred, rf_pred, greedy_pred)
  ensemble_model <- greedyMSE(ensemble_X, matrix(Y_binary, ncol = 1L))

  # Calculate AUC for each model
  calc_auc <- function(pred, actual) {
    pred_obj <- ROCR::prediction(pred, actual)
    perf_obj <- ROCR::performance(pred_obj, "auc")
    as.numeric(perf_obj@y.values[[1L]])
  }

  # Check if ensemble performs better than the best individual model
  individual_rmse <- c(
    sqrt(mean((Y_binary - glm_pred)^2L)),
    sqrt(mean((Y_binary - rpart_pred)^2L)),
    sqrt(mean((Y_binary - rf_pred)^2L)),
    greedy_model$RMSE
  )
  testthat::expect_lte(ensemble_model$RMSE, min(individual_rmse))
})
