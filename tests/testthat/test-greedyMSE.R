# Helper function to create a simple dataset
create_dataset <- function(n_samples, n_features = 5L, n_targets = 1L, coef = NULL, noise = 0L) {
  X <- matrix(stats::runif(n_samples * n_features), nrow = n_samples)
  colnames(X) <- paste0("Feature", seq_len(n_features))

  if (is.null(coef)) {
    coef <- matrix(stats::runif(n_features * n_targets), nrow = n_features)
  } else if (is.vector(coef)) {
    coef <- matrix(coef, ncol = 1L)
  }

  # Normalize coefficients
  coef <- apply(coef, 2L, function(col) col / sum(abs(col)))

  Y <- X %*% coef + matrix(stats::rnorm(n_samples * n_targets, 0.0, noise), nrow = n_samples)
  colnames(Y) <- letters[seq_len(ncol(Y))]

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
N <- 100L
regression_data <- create_dataset(N, noise = 0.1)
multi_regression_data <- create_dataset(N, n_targets = 3L)

Y_binary <- matrix(as.integer(regression_data$Y > mean(regression_data$Y)), nrow = N)
Y_multi_binary <- matrix(as.integer(multi_regression_data$Y > mean(multi_regression_data$Y)), nrow = N)
colnames(Y_multi_binary) <- letters[seq_len(ncol(Y_multi_binary))]

# Test for regression (one col)
testthat::test_that("greedyMSE works for regression", {
  model <- greedyMSE(regression_data$X, regression_data$Y)
  testthat::expect_lt(model$RMSE, stats::sd(regression_data$Y)) # Model should be better than baseline
  # High correlation with true values
  testthat::expect_gt(stats::cor(predict(model, regression_data$X), regression_data$Y), 0.8)

  testthat::expect_output(print(model), "Greedy MSE")
  testthat::expect_output(print(model), "RMSE")
  testthat::expect_output(print(model), "Weights")
})

# Test for binary classification (one col)
testthat::test_that("greedyMSE works for binary classification", {
  model <- greedyMSE(regression_data$X, Y_binary)
  predictions <- predict(model, regression_data$X)
  accuracy <- mean((predictions > 0.5) == Y_binary)
  testthat::expect_gt(accuracy, 0.7) # Accuracy should be better than random guessing

  testthat::expect_output(print(model), "Greedy MSE")
  testthat::expect_output(print(model), "RMSE")
  testthat::expect_output(print(model), "Weights")
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
  testthat::expect_gt(cor(pred, Y), 0.4)
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

  # Check if ensemble performs better than the best individual model
  individual_rmse <- c(
    sqrt(mean((Y_binary - glm_pred)^2L)),
    sqrt(mean((Y_binary - rpart_pred)^2L)),
    sqrt(mean((Y_binary - rf_pred)^2L)),
    greedy_model$RMSE
  )
  testthat::expect_lte(ensemble_model$RMSE, min(individual_rmse))
})

# Test fitting and predicting with factor response (2 levels)
testthat::test_that("greedyMSE works with 2-level factor response", {
  lev <- c("Low", "High")
  y_factor <- cut(regression_data$Y, breaks = 2L, labels = lev)
  model <- greedyMSE(regression_data$X, y_factor)
  pred <- predict(model, regression_data$X, return_labels = TRUE)

  testthat::expect_is(pred, "factor")
  testthat::expect_identical(levels(pred), levels(y_factor))
  testthat::expect_gt(mean(pred == y_factor), 0.60) # 0.50 is random guessing
})

# Test fitting and predicting with factor response (3 levels)
testthat::test_that("greedyMSE works with 3-level factor response", {
  lev <- c("Low", "Medium", "High")
  y_factor <- cut(regression_data$Y, breaks = 3L, labels = lev)
  model <- greedyMSE(regression_data$X, y_factor)
  pred <- predict(model, regression_data$X, return_labels = TRUE)

  testthat::expect_is(pred, "factor")
  testthat::expect_identical(levels(pred), levels(y_factor))
  testthat::expect_gt(mean(pred == y_factor), 0.4) # 0.33 is random guessing
})

# Test varImp functionality
testthat::test_that("varImp works for greedyMSE", {
  model <- greedyMSE(regression_data$X, regression_data$Y)
  importance <- caret::varImp(model)

  testthat::expect_is(importance, "data.frame")
  testthat::expect_identical(nrow(importance), ncol(regression_data$X))
  testthat::expect_true(all(importance$Overall >= 0.0 & importance$Overall <= 1.0))
  testthat::expect_equal(sum(importance$Overall), 1.0, tolerance = 1e-6)
})

# Test predict with data.frame input
testthat::test_that("predict works with data.frame input", {
  model <- greedyMSE(regression_data$X, regression_data$Y)
  newdata_df <- as.data.frame(regression_data$X)
  pred <- predict(model, newdata_df)

  testthat::expect_is(pred, "numeric")
  testthat::expect_length(pred, nrow(newdata_df))
})

# Test predict with label return for classification
testthat::test_that("predict returns labels for classification", {
  y_factor <- factor(ifelse(regression_data$Y > median(regression_data$Y), "High", "Low"))
  model <- greedyMSE(regression_data$X, y_factor)
  pred_prob <- predict(model, regression_data$X, return_labels = FALSE)
  pred_label <- predict(model, regression_data$X, return_labels = TRUE)

  testthat::expect_is(pred_prob, "matrix")
  testthat::expect_is(pred_label, "factor")
  testthat::expect_identical(levels(pred_label), levels(y_factor))
})

# More realistic test
testthat::test_that("greedyMSE handles highly correlated predictors", {
  set.seed(123L)
  n <- 1000L
  base <- rnorm(n)
  X <- matrix(
    cbind(
      base + rnorm(n, 0.0, 0.01),
      base + rnorm(n, 0.0, 0.01),
      base + rnorm(n, 0.0, 0.01),
      base + rnorm(n, 0.0, 0.01),
      base + rnorm(n, 0.0, 0.01),
      rnorm(n)
    ),
    ncol = 6L
  )
  colnames(X) <- c("x1", "x2", "x3", "x4", "x5", "x6")
  coef <- c(10.0, 10.0, 5.0, 5.0, 0.0, 0.0)
  coef <- coef / sum(coef)
  Y <- X %*% coef + rnorm(n, 0.0, 0.1)
  Y <- Y - mean(Y)
  Y <- Y / sd(Y)

  model <- greedyMSE(X, Y)
  pred <- predict(model, X)

  rmse_model <- sqrt(mean((pred - Y)^2L))
  rmse_mean <- sqrt(mean((mean(Y) - Y)^2L))

  testthat::expect_lt(rmse_model, rmse_mean)
})

testthat::test_that("greedyMSE works with caret::train for regression", {
  set.seed(42L)
  n <- 1000L
  X1 <- stats::rnorm(n)
  X2 <- 0.5 * X1 + sqrt(1L - 0.5^2L) * stats::rnorm(n)
  Y <- 0.9 * X1 + 0.9 * X2 + 0.1 * stats::rnorm(n)
  Y <- (Y - mean(Y)) / stats::sd(Y) # Standardize Y

  # Split data
  train_indices <- sample.int(n, 0.7 * n)
  X_train <- cbind(X1, X2)[train_indices, ]
  X_test <- cbind(X1, X2)[-train_indices, ]
  Y_train <- Y[train_indices]
  Y_test <- Y[-train_indices]

  # Train model
  model <- caret::train(
    X_train,
    Y_train,
    tuneLength = 1L,
    method = greedyMSE_caret()
  )

  # Make predictions
  predictions <- predict(model, newdata = X_test)

  # Compute RMSE
  rmse_model <- sqrt(mean((predictions - Y_test)^2L))
  rmse_mean <- sqrt(mean((mean(Y_train) - Y_test)^2L))

  # Check model performance
  testthat::expect_lt(rmse_model, rmse_mean)

  # Check variable importance
  w <- model$finalModel$model_weights
  expect_equal(sum(w), 1.0, tol = 1.0e-6)
  expect_equal(w[1L], w[2L], tol = 0.2)
})

testthat::test_that("greedyMSE works with caret::train for binary classification", {
  set.seed(42L)

  # Make Y
  n <- 1000L
  X1_pos <- stats::runif(n)
  X2_pos <- 0.5 * X1_pos + sqrt(1L - 0.5^2L) * stats::runif(n)
  Y <- 0.9 * X1_pos + 0.9 * X2_pos + 0.05 * stats::rnorm(n)
  Y <- cut(Y, breaks = 2L, labels = c("Low", "High"))

  # Add negative classes
  # THIS IS IMPORTANT!
  # We need probabilities from ALL classes for ALL the input models
  X1_neg <- 1.0 - X1_pos
  X2_neg <- 1.0 - X2_pos

  X <- cbind(X1_pos, X2_pos, X1_neg, X2_neg)

  # Split data
  train_indices <- sample.int(n, 0.7 * n)
  X_train <- X[train_indices, ]
  X_test <- X[-train_indices, ]
  Y_train <- Y[train_indices]
  Y_test <- Y[-train_indices]

  # Train model
  model <- caret::train(
    X_train,
    Y_train,
    tuneLength = 1L,
    method = greedyMSE_caret()
  )

  # Make predictions
  predictions <- predict(model, newdata = X_test, type = "prob")

  # Compute AUC
  auc <- mean(caTools::colAUC(predictions, Y_test))
  benchmark_auc <- max(caTools::colAUC(X_test, Y_test))

  # Check model performance
  testthat::expect_gt(auc, benchmark_auc)
})

testthat::test_that("greedyMSE works with caret::train for multiclass classification", {
  set.seed(42L)
  n <- 1000L

  # Generate base correlated variables
  Z1 <- stats::rnorm(n)
  Z2 <- 0.5 * Z1 + sqrt(2L - 0.5^2L) * stats::rnorm(n)

  # Generate Y with high correlation to Z1 and Z2
  Y_numeric <- 0.9 * Z1 + 0.9 * Z2 + stats::rnorm(n)

  # Convert Y to categorical
  Y <- cut(Y_numeric, breaks = 3L, labels = c("Low", "Medium", "High"))

  # Create probability matrices for X1 and X2
  create_prob_matrix <- function(z) {
    probs <- stats::pnorm(z, mean = mean(z), sd = sd(z))
    raw_matrix <- matrix(c(1L - probs, probs - 0.5, 0.5 * probs), ncol = 3L)
    exp_matrix <- exp(raw_matrix)
    exp_matrix / rowSums(exp_matrix)
  }

  X1 <- create_prob_matrix(Z1)
  X2 <- create_prob_matrix(Z2)

  # Combine X1 and X2
  X <- cbind(X1, X2)
  colnames(X) <- c("X1_Low", "X1_Medium", "X1_High", "X2_Low", "X2_Medium", "X2_High")

  # Split data
  train_indices <- sample.int(n, 0.7 * n)
  X_train <- X[train_indices, ]
  X_test <- X[-train_indices, ]
  Y_train <- Y[train_indices]
  Y_test <- Y[-train_indices]

  # Train model
  model <- caret::train(
    X_train,
    Y_train,
    tuneLength = 1L,
    method = greedyMSE_caret()
  )

  # Make predictions
  predictions <- predict(model, newdata = X_test, type = "prob")

  # Compute AUC
  auc <- rowMeans(caTools::colAUC(predictions, Y_test))
  benchmark_auc <- rowMeans(caTools::colAUC(X_test, Y_test))

  # Check model performance
  testthat::expect_true(all(auc > benchmark_auc))
})
