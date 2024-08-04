#' @title Normalize to One
#' @description Normalize a vector to sum to one.
#' @param x
#' @return A numeric vector.
#' @keywords internal
normalize_to_one <- function(x) {
  x <- abs(x)
  x_sum <- sum(x)
  if (x_sum == 0.0) {
    return(rep(1.0 / length(x), length(x)))
  }
  x / x_sum
}

#' @title Compute MAE
#' @description Compute the mean absolute error between two vectors.
#' @param a A numeric vector.
#' @param b A numeric vector.
#' @param log_odds A logical indicating whether the vectors should be converted to log odds
#' @param delta If log_odds, the probabilities will be capped at delta and 1 - delta
#' @return A numeric scalar.
#' @keywords internal
mae <- function(a, b, log_odds = FALSE, delta = 1e-8) {
  if (log_odds) {
    a <- pmax(delta, pmin(1.0 - delta, a))
    b <- pmax(delta, pmin(1.0 - delta, b))

    a <- qlogis(a)
    b <- qlogis(b)
  }
  mean(abs(a - b))
}

#' @title Is Classifier
#' @description Check if a model is a classifier.
#' @param model A train object from the caret package.
#' @return A logical indicating whether the model is a classifier.
#' @keywords internal
isClassifier <- function(model) {
  stopifnot(methods::is(model, "train") || methods::is(model, "caretStack"))
  if (methods::is(model, "train")) {
    out <- model$modelType == "Classification"
  } else {
    out <- model$ens_model$modelType == "Classification"
  }
  out
}

#' @title Target to Matrix
#' @description Convert a target variable to a matrix.
#' @param target A vector of target values.
#' @param is_class A logical indicating whether the target is for a classifier or regressor
#' @return A matrix.
#' @keywords internal
target_to_matrix <- function(target, is_class, levels) {
  if (is_class) {
    stopifnot(is.factor(target) || is.character(target))
    if (is.character(target)) {
      target <- factor(target, levels)
    }
    target <- model.matrix(~ 0L + target)
  }
  target <- as.matrix(target)
  stopifnot(
    is.numeric(target),
    is.finite(target),
    length(dim(target)) == 2L
  )
  target
}

#' @title Permutation Importance
#' @description Permute each varible in a dataset and use the change in predictions to
#' calculate the importance of each variable. Based on sci-kit learn's implementation
#' of permutation importance: https://scikit-learn.org/stable/modules/permutation_importance.html
#' how do I do a link in roxygen2?  https://stackoverflow.com/questions/27731400/how-to-create-a-link-in-roxygen2
#' @param model A train object from the caret package.
#' @param newdata A data.frame of new data to use to compute importances.  Can be the training data.
#' @param target The target variable.
#' @param normalize A logical indicating whether to normalize the importances to sum to one.
#' @return A named numeric vector of variable importances.
#' @export
permutationImportance <- function(model, newdata, target, normalize = TRUE) {
  # Checks
  stopifnot(
    methods::is(model, "train") || methods::is(model, "caretStack"),
    methods::is(newdata, "data.frame")
  )

  # Make a copy of the data, since we'll be shuffling it
  # If this process dies partway through, we don't want
  # to have randomly modidifed the original data in place
  N <- nrow(newdata)
  original_data <- data.table::as.data.table(data.table::copy(newdata))
  shuffled_data <- data.table::as.data.table(data.table::copy(original_data))
  keyname <- "aca75a39eb385d7de8d9caef41ec0521442f499211fee946f03835c57ee33d35"
  shuffle_idx <- sample.int(N)
  data.table::set(shuffled_data, j = keyname, value = shuffle_idx)
  data.table::setkeyv(shuffled_data, keyname)
  data.table::set(shuffled_data, j = keyname, value = NULL)

  # Turn class target into a matrix
  is_class <- isClassifier(model)
  pred_type <- ifelse(is_class, "prob", "raw")
  target <- target_to_matrix(target, is_class, levels = levels(model))
  stopifnot(N == nrow(target))

  # Error of the original model.
  # This is the baseline for computing importance
  preds_orig <- as.matrix(predict(model, newdata, type = pred_type))
  stopifnot(
    is.numeric(preds_orig),
    is.finite(preds_orig),
    dim(preds_orig) == dim(target),
    paste0("target", colnames(preds_orig)) == colnames(target)
  )
  mae_model <- mae(preds_orig, target)

  # Error for each variable
  # Loop through each variable, shuffle it, and calculate mae of the new predictions
  mae_vars <- vapply(names(newdata), function(var) {
    old_var <- data.table::copy(original_data[[var]])
    new_var <- data.table::copy(shuffled_data[[var]])

    data.table::set(original_data, j = var, value = new_var)
    new_preds <- as.matrix(predict(model, original_data, type = pred_type))
    data.table::set(original_data, j = var, value = old_var)

    mae(new_preds, target)
  }, numeric(1L))

  # Error predicting all zeros
  mae_zero <- mae(0L, target)

  # Normalize the errors into importances
  # If the mae for a variable is equal to the mae of 0
  # That means most of the predictive power of the model
  # comes from that variable. On the other hand, if the
  # mae for a variable is close to zero it means the variable
  # is not important.
  imp <- (mae_vars - mae_model) / (mae_zero - mae_model)
  if (normalize) imp <- normalize_to_one(imp)
  imp
}
