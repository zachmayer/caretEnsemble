#' @title Greedy optimization for MSE
#' @description Greedy optimization for minimizing the mean squared error.
#' Works for classification and regression.
#' @param X A numeric matrix of features.
#' @param Y A numeric matrix of target values.
#' @param max_iter An integer scalar of the maximum number of iterations.
#' @return A list with components:
#' \item{model_weights}{A numeric matrix of model_weights.}
#' \item{RMSE}{A numeric scalar of the root mean squared error.}
#' \item{max_iter}{An integer scalar of the maximum number of iterations.}
#' @export
greedyMSE <- function(X, Y, max_iter = 100L) {
  # X to matrix
  X <- if (is.matrix(X)) X else as.matrix(X)

  # Y to matrix
  if (is.matrix(Y)) {
    y_matrix <- Y
  } else if (is.factor(Y)) {
    lev <- levels(Y)
    y_matrix <- matrix(
      0.0,
      nrow = length(Y),
      ncol = length(lev),
      dimnames = list(NULL, lev)
    )
    for (i in seq_along(lev)) {
      y_matrix[, i] <- as.integer(Y == lev[i])
    }
    colnames(y_matrix) <- lev
  } else {
    y_matrix <- matrix(Y, ncol = 1L)
  }

  # Checks
  stopifnot(
    is.matrix(X), is.matrix(y_matrix),
    is.numeric(X), is.numeric(y_matrix),
    is.finite(X), is.finite(y_matrix),
    nrow(X) == nrow(y_matrix),
    ncol(X) >= 1L, ncol(y_matrix) >= 1L,
    is.integer(max_iter), max_iter > 0L
  )

  # Initialize empty weights and the potential weight updates
  # The diag matrix basically proposes updating each weight by 1L
  n_targets <- ncol(y_matrix)
  model_weights <- matrix(0L, nrow = ncol(X), ncol = n_targets)
  model_update <- diag(ncol(X))

  for (iter in seq_len(max_iter)) {
    for (y_col in seq_len(n_targets)) {
      target <- y_matrix[, y_col]
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
  colnames(model_weights) <- colnames(y_matrix)
  RMSE <- sqrt(mean((X %*% model_weights - y_matrix)^2.0))
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
#' @method print greedyMSE
#' @export
print.greedyMSE <- function(x, ...) {
  cat("Greedy MSE\n")
  cat("RMSE: ", x$RMSE, "\n")
  cat("Weights:\n")
  print(x$model_weights)
}

#' @title variable importance for a greedyMSE model
#' @description Variable importance for a greedyMSE model.
#' @param object A greedyMSE object.
#' @param ... Additional arguments. Ignored.
#' @importFrom caret varImp
#' @method varImp greedyMSE
#' @export
varImp.greedyMSE <- function(object, ...) {
  importance <- rowSums(abs(object$model_weights))
  importance <- importance / sum(importance)
  out <- data.frame(Overall = importance)
  rownames(out) <- row.names(object$model_weights)
  out
}

#' @title Predict method for greedyMSE
#' @description Predict method for greedyMSE objects.
#' @param object A greedyMSE object.
#' @param newdata A numeric matrix of new data.
#' @param return_labels A logical scalar of whether to return labels.
#' @param ... Additional arguments. Ignored.
#' @return A numeric matrix of predictions.
#' @export
predict.greedyMSE <- function(object, newdata, return_labels = FALSE, ...) {
  newdata <- if (is.matrix(newdata)) newdata else as.matrix(newdata)
  stopifnot(
    is.numeric(newdata),
    is.finite(newdata),
    ncol(newdata) == nrow(object$model_weights)
  )

  pred <- newdata %*% object$model_weights
  if (ncol(pred) > 1L) {
    if (return_labels) {
      lev <- colnames(object$model_weights)
      pred <- lev[apply(pred, 1L, which.max)]
      pred <- factor(pred, levels = lev)
    } else {
      pred <- pred / rowSums(pred)
    }
  } else {
    pred <- pred[, 1L]
  }

  pred
}

#' @title caret interface for greedyMSE
#' @description caret interface for greedyMSE. greedyMSE works
#' well when you want an ensemble that will never be worse than any single predictor
#' in the dataset. It does not use an intercept and it does not allow for
#' negative coefficients. This makes it highly constrained and in general
#' does not work well on standard classification and regression problems.
#' However, it does work well in the case of:
#' * The predictors are highly correlated with each other
#' * The predictors are highly correlated with the model
#' * You expect or want positive only coefficients
#' In the worse case, this method will select one input and use that,
#' but in many other cases it will return a positive, weighted average
#' of the inputs. Since it never uses negative weights, you never get
#' into a scenario where one model is weighted negative and on new data
#' you get were predictions because a correlation changed.
#' Since this model will always be a positive weighted average of the inputs,
#' it will rarely do worse than the individual models on new data.
#' @export
greedyMSE_caret <- function() {
  list(
    label = "Greedy Mean Squared Error Optimizer",
    method = "greedyMSE",
    library = NULL,
    loop = NULL,
    type = c("Regression", "Classification"),
    parameters = data.frame(
      parameter = "max_iter",
      class = "integer",
      label = "Max Iterations",
      stringsAsFactors = FALSE
    ),
    grid = function(x, y, len = 1L, search = "grid") {
      data.frame(max_iter = as.integer(floor(seq.int(100L, 250L, length.out = len))))
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      greedyMSE(X = x, Y = y, max_iter = param$max_iter)
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      stats::predict(modelFit, newdata, return_labels = modelFit$problemType == "Classification")
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      stats::predict(modelFit, newdata, return_labels = FALSE)
    },
    tags = c("Greedy Optimizer", "Mean Squared Error", "Interpretable"),
    sort = function(x) x[order(x$max_iter), ]
  )
}
