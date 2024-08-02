#' @title Normalize to One
#' @description Normalize a vector to sum to one.
#' @param x
#' @return A numeric vector.
#' @keywords internal
normalize_to_one <- function(x) {
  x / sum(x)
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

#' @title Permutation Importance
#' @description Permute each varible in a dataset and use the change in predictions to
#' calculate the importance of each variable.
#' @param model A train object from the caret package.
#' @param newdata A data.frame of new data to use to compute importances.  Can be the training data.
#' @param target The target variable.
#' @return A named numeric vector of variable importances.
#' @export
permutationImportance <- function(model, newdata) {
  # Checks
  stopifnot(
    methods::is(model, "train") || methods::is(model, "caretStack"),
    methods::is(newdata, "data.frame")
  )
  is_class <- isClassifier(model)

  # Make a copy of the data, since we'll be shuffling it
  # If this process dies partway through, we don't want
  # to have randomly modidifed the original data in place
  newdata <- data.table::as.data.table(data.table::copy(newdata[1L, ]))

  # Make the original predictions
  preds_orig <- as.matrix(predict(model, newdata, type = ifelse(is_class, "prob", "raw")))
  stopifnot(
    is.numeric(preds_orig),
    is.finite(preds_orig)
  )

  # Find the intercept
  newdata_zero <- data.table::as.data.table(data.table::copy(newdata))
  for (var in names(newdata_zero)) {
    data.table::set(newdata_zero, j = var, value = 0.0)
  }
  pred_zero <- predict(model, newdata_zero, type = ifelse(is_class, "prob", "raw"))
  pred_zero <- as.matrix(pred_zero)
  intercept <- sqrt(mean((pred_zero)^2L))

  # Find each "coefficient"
  CFs <- vapply(names(newdata), function(var) {
    old_var <- data.table::copy(newdata[[var]])
    data.table::set(newdata, j = var, value = 0.0)
    new_preds <- as.matrix(predict(model, newdata, type = ifelse(is_class, "prob", "raw")))
    data.table::set(newdata, j = var, value = old_var)
    sqrt(mean((new_preds - preds_orig)^2L))
  }, numeric(1L))

  # Add the intercept to the model errors
  imp <- c(intercept = intercept, CFs)
  imp <- normalize_to_one(imp)
  imp
}
