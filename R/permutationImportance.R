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

#' @title Permutation Importance
#' @description Permute each varible in a dataset and use the change in predictions to
#' calculate the importance of each variable.
#' @param model A train object from the caret package.
#' @param newdata A data.frame of new data to use to compute importances.  Can be the training data.
#' @param target The target variable.
#' @return A named numeric vector of variable importances.
#' @export
permutationImportance <- function(model, newdata, target) {
  # Checks
  stopifnot(
    methods::is(model, "train") || methods::is(model, "caretStack"),
    methods::is(newdata, "data.frame")
  )

  # Make a copy of the data, since we'll be shuffling it
  # If this process dies partway through, we don't want
  # to have randomly modidifed the original data in place
  newdata <- data.table::as.data.table(data.table::copy(newdata))
  N <- nrow(newdata)

  # Turn class target into a matrix
  is_class <- isClassifier(model)
  if (is_class) {
    stopifnot(is.factor(target) || is.character(target))
    if (is.character(target)) {
      target <- factor(target, levels = levels(model))
    }
    target <- model.matrix(~ 0L + target)
  }
  target <- as.matrix(target)
  stopifnot(
    is.numeric(target),
    is.finite(target),
    length(dim(target)) == 2L
  )

  # Error of the NULL model
  TSS <- sqrt(mean((0L - target)^2L))

  # Error of the NULL model
  preds_mean <- matrix(colMeans(target), nrow = N, ncol = ncol(target), byrow = TRUE)
  SEE_intercept <- sqrt(mean((0L - preds_mean)^2L))

  # Error of the original model
  # aka SSM or Sum of Squares Model
  preds_orig <- as.matrix(predict(model, newdata, type = ifelse(is_class, "prob", "raw")))
  stopifnot(
    is.numeric(preds_orig),
    is.finite(preds_orig),
    dim(preds_orig) == dim(target),
    paste0("target", colnames(preds_orig)) == colnames(target)
  )

  # Error of permuting each variable, or SSE
  # Loop through each variable, shuffle it, and calculate RMSE of the new predictions
  shuffle_idx <- sample.int(N)
  SSE <- vapply(names(newdata), function(var) {
    old_var <- data.table::copy(newdata[[var]])
    data.table::set(newdata, j = var, value = old_var[shuffle_idx])
    new_preds <- as.matrix(predict(model, newdata, type = ifelse(is_class, "prob", "raw")))
    data.table::set(newdata, j = var, value = old_var)
    sqrt(mean((new_preds - target)^2L))
  }, numeric(1L))

  # Add the intercept to the model errors
  imp <- c(intercept = SEE_intercept, SSE) / TSS
  imp <- imp / sum(imp)
  imp
}
