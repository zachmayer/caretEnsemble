#' @title Greedy optimization for MSE
#' @description Greedy optimization for minimizing the mean squared error.
#' Works for classificaton and regression.
#' @param X A numeric matrix of features.
#' @param Y A numeric matrix of target values.
#' @param max_iter An integer scalar of the maximum number of iterations.
#' @return A list with components:
#' \item{model_weights}{A numeric matrix of model_weights.}
#' \item{RMSE}{A numeric scalar of the root mean squared error.}
#' \item{max_iter}{An integer scalar of the maximum number of iterations.}
#' @export
greedyMSE <- function(X, Y, max_iter = 100L) {
  stopifnot(
    is.matrix(X), is.matrix(Y),
    is.numeric(X), is.numeric(Y),
    is.finite(X), is.finite(Y),
    nrow(X) == nrow(Y), ncol(X) >= 1L, ncol(Y) >= 1L,
    is.integer(max_iter), max_iter > 0L
  )

  model_weights <- matrix(0L, nrow = ncol(X), ncol = ncol(Y))
  model_update <- diag(ncol(X))

  for (iter in seq_len(max_iter)) {
    for (y_col in seq_len(ncol(Y))) {
      target <- Y[, y_col]
      w <- model_weights[, y_col]

      # Calculate MSE for incrementing each weight
      w_new <- w + model_update
      w_new <- w_new / colSums(w_new)
      predictions <- X %*% w_new
      MSE <- colMeans((predictions - target)^2.0)

      # Update the best weight
      best_id <- which.min(MSE)
      model_weights[best_id, y_col] <- model_weights[best_id, y_col] + 1L
    }
  }

  # Output
  model_weights <- model_weights / colSums(model_weights)
  rownames(model_weights) <- colnames(X)
  colnames(model_weights) <- colnames(Y)
  RMSE <- sqrt(mean((X %*% model_weights - Y)^2.0))
  out <- list(
    model_weights = model_weights,
    RMSE = RMSE,
    max_iter = max_iter
  )
  class(out) <- "greedyMSE"
  out
}

#' @title Print method for greedyMSE
#' @description Print method for greedyMSE objects.
#' @param x A greedyMSE object.
#' @param ... Additional arguments. Ignored.
#' @export
print.greedyMSE <- function(x, ...) {
  cat("Greedy MSE\n")
  cat("RMSE: ", x$RMSE, "\n")
  cat("model_weights:\n")
  print(x$model_weights)
}

#' @title Predict method for greedyMSE
#' @description Predict method for greedyMSE objects.
#' @param object A greedyMSE object.
#' @param newdata A numeric matrix of new data.
#' @param ... Additional arguments. Ignored.
#' @return A numeric matrix of predictions.
#' @export
predict.greedyMSE <- function(object, newdata, ...) {
  stopifnot(
    is.matrix(newdata),
    is.numeric(newdata),
    is.finite(newdata),
    ncol(newdata) == nrow(object$model_weights)
  )
  out <- newdata %*% object$model_weights
  if (ncol(out) > 1L) {
    out <- out / rowSums(out)
  }
  out
}
