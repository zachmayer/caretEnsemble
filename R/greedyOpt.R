greedyMSE <- function(X, Y, levels = NULL, max_iter = 100L) {
  stopifnot(
    is.matrix(X), is.matrix(Y),
    is.numeric(X), is.numeric(Y),
    all(is.finite(X)), all(is.finite(Y)),
    nrow(X) == nrow(Y), ncol(X) >= 1L, ncol(Y) >= 1L,
    is.integer(max_iter), max_iter > 0L
  )

  weights <- matrix(0, nrow = ncol(X), ncol = ncol(Y))
  weights_update <- diag(ncol(X))

  for (iter in 1L:max_iter) {
    for (y_col in 1L:ncol(Y)) {
      target <- Y[, y_col]
      w <- weights[, y_col]

      # Calculate MSE for incrementing each weight
      w_new <- w + weights_update
      w_new <- w_new / colSums(w_new)
      predictions <- X %*% w_new
      MSE <- colMeans((predictions - target)^2.0)

      # Update the best weight
      best_id <- which.min(MSE)
      weights[best_id, y_col] <- weights[best_id, y_col] + 1L
    }
  }

  # Output
  weights <- weights / colSums(weights)
  rownames(weights) <- colnames(X)
  colnames(weights) <- colnames(Y)
  RMSE <- sqrt(mean((X %*% weights - Y)^2.0))
  out <- list(
    weights = weights,
    RMSE = RMSE,
    max_iter = max_iter
  )
  class(out) <- "greedyMSE"
  out
}

print.greedyMSE <- function(x, ...) {
  cat("Greedy MSE\n")
  cat("RMSE: ", x$RMSE, "\n")
  cat("Weights:\n")
  print(x$weights)
}

predict.greedyMSE <- function(object, newdata, ...) {
  stopifnot(
    is.matrix(newdata),
    is.numeric(newdata),
    all(is.finite(newdata)),
    ncol(newdata) == nrow(object$weights)
  )
  out <- newdata %*% object$weights
  if (ncol(out) > 1L) {
    out <- out / rowSums(out)
  }
  out
}

set.seed(42)
X <- matrix(runif(200), nrow = 100)
Y <- X %*% c(6, 4) / 10

X <- cbind(X, 1 - X)
colnames(X) <- c("yes1", "yes2", "no1", "no2")
Y <- cbind(Y, 1 - Y)
colnames(Y) <- paste0("Y", 1:ncol(Y))

model <- greedyMSE(X, Y)
print(model)
predict(model, X)
