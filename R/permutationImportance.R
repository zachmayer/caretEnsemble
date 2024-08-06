#' @title Normalize to One
#' @description Normalize a vector to sum to one.
#' @param x A numeric vector.
#' @return A numeric vector.
#' @keywords internal
normalize_to_one <- function(x) {
  x <- abs(x)
  x_sum <- sum(x)
  if (x_sum == 0.0) {
    out <- rep(1.0 / length(x), length(x))
    names(out) <- names(x)
  } else {
    out <- x / x_sum
  }
  out
}

#' @title Compute MAE
#' @description Compute the mean absolute error between two vectors.
#' @param a A numeric vector.
#' @param b A numeric vector.
#' @return A numeric scalar.
#' @keywords internal
mae <- function(a, b) {
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


#' @title Shuffled MAE
#' @description Compute the mean absolute error of a model's predictions when a variable is shuffled.
#' @param original_data A data.table of the original data.
#' @param target A matrix of target values.
#' @param shuffle_idx A vector of shuffled indices.
#' @return A numeric vector of mean absolute errors.
#' @keywords internal
shuffled_mae <- function(model, original_data, target, pred_type, shuffle_idx) {
  # Make a copy of the data, since we'll be shuffling it
  # If this process dies partway through, we don't want
  # to have randomly modidifed the original data in place
  original_data <- data.table::as.data.table(data.table::copy(original_data))
  shuffled_data <- data.table::as.data.table(data.table::copy(original_data))

  keyname <- "aca75a39eb385d7de8d9caef41ec0521442f499211fee946f03835c57ee33d35"
  data.table::set(shuffled_data, j = keyname, value = shuffle_idx)
  data.table::setkeyv(shuffled_data, keyname)
  data.table::set(shuffled_data, j = keyname, value = NULL)

  # Error for each variable
  # Loop through each variable, shuffle it, and calculate mae of the new predictions
  mae_vars <- vapply(names(original_data), function(var) {
    old_var <- original_data[[var]]
    new_var <- shuffled_data[[var]]

    data.table::set(original_data, j = var, value = new_var)
    new_preds <- as.matrix(stats::predict(model, original_data, type = pred_type))
    data.table::set(original_data, j = var, value = old_var)

    mae(new_preds, target)
  }, numeric(1L))

  mae_vars
}

#' @title Permutation Importance
#' @description Permute each variable in a dataset and use the change in predictions to
#' calculate the importance of each variable. Based on the scikit learn implementation
#' of permutation importance: \url{https://scikit-learn.org/stable/modules/permutation_importance.html}.
#' However, we don't compare to the target by a metric. We JUST look at the change in the
#' model's predictions, as measured by MAE. (for classification, this is like using a Brier score).
#' We shuffle each variable and recompute the predictions before and after the shuffle.
#' The difference in MAE. is the importance of that variable. We normalize by computing the MAE of the shuffled
#' original predictions as an upper bound on the MAE and divide by this value.
#' So a variable that, when shuffled, caused predictions as bad as shuffling the output
#' predictions, we know that variable is 100% of the model's predictive power.
#' Similarly, as with regular permutation importance, a variable that, when shuffled,
#' gives the same MAE as the original model has an importance of 0.
#'
#' This method cannot yield negative importances. It is merely a measure of how much the models uses
#' the variable, and does not tell you which variables help or hurt generalization. Use the model's
#' cross-validated metrics to assess generalization.
#' @param model A train object from the caret package.
#' @param newdata A data.frame of new data to use to compute importances. Can be the training data.
#' @param normalize A logical indicating whether to normalize the importances to sum to one.
#' @return A named numeric vector of variable importances.
#' @export
permutationImportance <- function(
    model,
    newdata,
    normalize = TRUE) {
  # Checks
  stopifnot(
    methods::is(model, "train") || methods::is(model, "caretStack"),
    methods::is(newdata, "data.frame")
  )

  is_class <- isClassifier(model)
  pred_type <- ifelse(is_class, "prob", "raw")

  N <- nrow(newdata)
  shuffle_idx <- sample.int(N)

  # Error of the original model.
  # This is the baseline for computing importance
  preds_orig <- as.matrix(stats::predict(model, newdata, type = pred_type))
  stopifnot(
    is.numeric(preds_orig),
    is.finite(preds_orig)
  )

  # Error of shuffled variables
  mae_vars <- shuffled_mae(model, newdata, preds_orig, pred_type, shuffle_idx)

  # Error from random predictions with no model
  # This is sort of the intercept.
  # This is basically the worst the model can be
  # But still uses the distribution of the predictions
  mae_no_model <- mae(preds_orig[shuffle_idx, ], preds_orig)
  if (mae_no_model == 0.0) mae_no_model <- 1.0

  # Normalize the errors into importances
  # If the mae for a variable is equal to the mae of 0
  # That means most of the predictive power of the model
  # comes from that variable. On the other hand, if the
  # mae for a variable is close to zero it means the variable
  # is not important.
  imp <- mae_vars / mae_no_model
  if (normalize) imp <- normalize_to_one(imp)
  imp
}
