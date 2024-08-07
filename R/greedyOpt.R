
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
  
  # Normalize weights
  weights <- apply(weights, 2, function(w) w / sum(w))
  weights
}

set.seed(42)
X <- matrix(runif(500), nrow = 100)
Y <- X %*% (cbind(c(0, 1, 6, 3, 0), c(0, 0, 0, 0, 10)) / 10)
max_iter <- 100L

greedyMSE(X, Y, 100L)