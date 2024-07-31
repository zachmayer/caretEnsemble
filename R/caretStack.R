#' @title Combine several predictive models via stacking
#'
#' @description Stack several \code{\link[caret]{train}} models using a \code{\link[caret]{train}} model.
#'
#' @details Uses either transfer learning or stacking to stack models. Assumes that all models were trained on
#' the same number of rows of data, with the same target values. The features, cross-validation strategies,
#' and model types (class vs reg) may vary however. If your stack of models were trained with different number of
#' rows, please provide new_X and new_y so the models can predict on a common set of data for stacking.
#'
#' If your models were trained on different columns, you should use stacking.
#'
#' If you have both differing rows and columns in your model set, you are out of luck. You need at least
#' a common set of rows during training (for stacking) or a common set of columns at
#' inference time for transfer learning.
#'
#' @param all.models a caretList, or an object coercible to a caretList (such as a list of train objects)
#' @param new_X Data to predict on for the caretList, prior to training the stack (for transfer learning).
#' if NULL, the stacked predictions will be extracted from the caretList models.
#' @param excluded_class_id The integer level to exclude from binary classification or multiclass problems.
#' @param new_y The outcome variable to predict on for the caretList, prior to training the stack
#' (for transfer learning).
#' If NULL, will use the observed levels from the first model in the caret stack
#' If 0, will include all levels.
#' @param ... additional arguments to pass to the stacking model
#' @return S3 caretStack object
#' @references Caruana, R., Niculescu-Mizil, A., Crew, G., & Ksikes, A. (2004).
#'   Ensemble Selection from Libraries of Models.
#'   \url{https://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
#' @export
#' @examples
#' \dontrun{
#' models <- caretList(
#'   x = iris[1:50, 1:2],
#'   y = iris[1:50, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' caretStack(models, method = "glm")
#' }
caretStack <- function(all.models, new_X = NULL, new_y = NULL, excluded_class_id = 1L, ...) {
  if (!is.caretList(all.models)) {
    warning("Attempting to coerce all.models to a caretList.")
    all.models <- as.caretList(all.models)
  }

  # Make sure either both or neither new_X and new_y are NULL
  if (is.null(new_X) != is.null(new_y)) {
    stop("Both new_X and new_y must be NULL, or neither.")
  }
  if (!is.null(new_X)) {
    stopifnot(
      is.data.frame(new_X) || is.matrix(new_X),
      is.numeric(new_y) || is.factor(new_y) || is.character(new_y),
      nrow(new_X) == length(new_y)
    )
    new_X <- data.table::as.data.table(new_X)
  }

  # Validators
  excluded_class_id <- validateExcludedClass(excluded_class_id)

  # Predict for each model. If new_X is NULL, will return stacked predictions
  preds <- predict.caretList(all.models, newdata = new_X, excluded_class_id = excluded_class_id)
  if (!is.null(new_X)) {
    stopifnot(nrow(preds) == nrow(new_X))
  }

  # Build a caret model
  obs <- new_y
  if (is.null(obs)) {
    obs <- data.table::data.table(all.models[[1L]]$pred)
    data.table::setorderv(obs, "rowIndex")
    obs <- obs[, list(obs = obs[1L]), by = "rowIndex"]
    obs <- obs[["obs"]]
  }
  stopifnot(nrow(preds) == length(obs))
  model <- caret::train(preds, obs, ...)

  # Return final model
  out <- list(models = all.models, ens_model = model, error = model$results, excluded_class_id = excluded_class_id)
  class(out) <- "caretStack"
  out
}

#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model predictions
#'
#' @param x a numeric vector
#' @param w a vector of weights equal to length of x
#' @param na.rm a logical indicating how to handle missing values, default = TRUE
#' @export
# https://stats.stackexchange.com/a/61285
wtd.sd <- function(x, w, na.rm = FALSE) {
  stopifnot(is.numeric(x), is.numeric(w))

  xWbar <- stats::weighted.mean(x, w, na.rm = na.rm)
  w <- w / mean(w, na.rm = na.rm)

  variance <- sum((w * (x - xWbar)^2L) / (sum(w, na.rm = na.rm) - 1L), na.rm = na.rm)
  out <- sqrt(variance)

  out
}

#' @title Make predictions from a caretStack
#' @description Make predictions from a caretStack. This function passes the data to each function in
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param object a  \code{\link{caretStack}} to make predictions from.
#' @param newdata a new dataframe to make predictions on
#' @param se logical, should prediction errors be produced? Default is false.
#' @param level tolerance/confidence level
#' @param return_weights a logical indicating whether prediction weights for each model
#' should be returned
#' @param excluded_class_id Which class to exclude from predictions. Note that if the caretStack
#' was trained with an excluded_class_id, that class is ALWAYS excluded from the predictions from the
#' caretList of input models. excluded_class_id for predict.caretStack is for the final ensemble model.
#' So different classes could be excluded from the caretList models and the final ensemble model.
#' @param return_class_only a logical indicating whether to return only the class predictions as a factor.
#' If TRUE, the return will be a factor rather than a data.table. This is a convenience function,
#' and should not be widely used. For example if you have a downstream process that consumes
#' the output of the model, you should have that process consume probabilities for each class.
#' This will make it easier to change prediction probability thresholds if needed in the future.
#' @param verbose a logical indicating whether to print progress
#' @param ... arguments to pass to \code{\link[caret]{predict.train}} for the ensemble model.
#' Do not specify type here. For classification, type will always be prob, and for regression, type will always be raw.
#' @return a data.table of predictions
#' @export
#' @details Prediction weights are defined as variable importance in the stacked
#' caret model. This is not available for all cases such as where the library
#' model predictions are transformed before being passed to the stacking model.
#' @importFrom stats predict
#' @method predict caretStack
#' @examples
#' \dontrun{
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' RMSE(predict(meta_model, iris[101:150, 1:2]), iris[101:150, 3])
#' }
predict.caretStack <- function(
    object,
    newdata = NULL,
    se = FALSE,
    level = 0.95,
    return_weights = FALSE,
    excluded_class_id = 0L,
    return_class_only = FALSE,
    verbose = FALSE,
    ...) {
  # Check the object
  stopifnot(
    methods::is(object$models, "caretList"),
    methods::is(object$ens_model, "train")
  )

  # Extract model types
  model_type <- object$ens_model$modelType

  # If the excluded class wasn't set at train time, set it
  if (model_type == "Classification" && is.null(object[["excluded_class_id"]])) {
    object[["excluded_class_id"]] <- 1L
    warning("No excluded_class_id set. Setting to 1L.")
  }

  # If we're predicting on new data, or we need standard errors, we need predictions from the submodels
  # Note that if se==TRUE and newdata is NULL, we will be returning STACKED predicitons
  if (!is.null(newdata)) {
    newdata <- data.table::as.data.table(newdata)
  }
  if (se || !is.null(newdata)) {
    preds <- predict(
      object$models,
      newdata = newdata,
      verbose = verbose,
      excluded_class_id = object[["excluded_class_id"]]
    )
    if (!is.null(newdata)) {
      newdata <- preds
    }
  }

  # Check return_class_only
  if (return_class_only) {
    stopifnot(
      model_type == "Classification",
      !se
    )
    excluded_class_id <- 0L
  }

  # Now predict on the stack
  # If newdata is NULL, this will be stacked predictions
  # If newdata is present, this will be predictions on the preds,
  # which will be the caretList predictions on the newdata.
  meta_preds <- caretPredict(object$ens_model, newdata = newdata, excluded_class_id = excluded_class_id, ...)
  out <- meta_preds

  # Map to class levels
  # TODO: HANDLE ORDINAL and TESTS FOR THIS
  if (return_class_only) {
    class_id <- apply(out, 1L, which.max)
    class_levels <- levels(object$ens_model)
    out <- factor(class_levels[class_id], class_levels)
  }

  # Calculate the model importances if we need them
  # For multiclass, this weights all classes evenly, which is... fine for now
  # In the future we could do SE by class, but that seems overcomplicated for now
  # As it is, this is pretty made up
  if (se || return_weights) {
    imp <- as.matrix(caret::varImp(object$ens_model)$importance)
    imp_names <- rownames(imp)
    imp <- apply(imp, 2L, function(x) x / sum(x))
    row.names(imp) <- imp_names
    imp <- rowMeans(imp)
    if (!all(
      length(imp) == ncol(preds),
      names(imp) %in% names(preds),
      names(preds) %in% names(imp)
    )) {
      warning("Cannot calculate standard errors due to the preprocessing used in train")
      se <- FALSE
    }
  }

  # Calculate SEs if we need them
  if (se) {
    std_error <- data.table::copy(preds)
    data.table::setcolorder(std_error, names(imp))
    std_error <- apply(std_error, 1L, wtd.sd, w = imp, na.rm = TRUE)
    std_error <- stats::qnorm(level) * std_error
    if (ncol(meta_preds) == 1L) {
      meta_preds <- meta_preds[[1L]] # No names if one column (e.g. reg or binary with a dropped class)
    }
    out <- data.table::data.table(
      fit = meta_preds,
      lwr = meta_preds - std_error,
      upr = meta_preds + std_error
    )
  }
  if (return_weights) {
    attr(out, "weights") <- imp
  }
  out
}

#' @title Check if an object is a caretStack object
#' @param object an R object
#' @description Check if an object is a caretStack object
#' @export
is.caretStack <- function(object) {
  methods::is(object, "caretStack")
}

#' @title Summarize a caretStack object
#' @description This is a function to summarize a caretStack.
#' @param object An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' summary(meta_model)
#' }
summary.caretStack <- function(object, ...) {
  summary(object$ens_model)
}

#' @title Print a caretStack object
#' @description This is a function to print a caretStack.
#' @param x An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' print(meta_model)
#' }
print.caretStack <- function(x, ...) {
  base.models <- paste(names(x$models), collapse = ", ")
  cat(sprintf("A %s ensemble of %s base models: %s", x$ens_model$method, length(x$models), base.models))
  cat("\n\nEnsemble results:\n")
  print(x$ens_model)
}

#' @title Plot a caretStack object
#' @description This is a function to plot a caretStack.
#' @param x An object of class caretStack
#' @param ... passed to plot
#' @export
#' @method plot caretStack
#' @examples
#' \dontrun{
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "rpart", tuneLength = 2)
#' plot(meta_model)
#' }
plot.caretStack <- function(x, ...) {
  plot(x$ens_model, ...)
}

#' @title Comparison dotplot for a caretStack object
#' @description This is a function to make a dotplot from a caretStack. It uses dotplot from the
#' caret package on all the models in the ensemble, excluding the final ensemble model.At the moment,
#' this function only works if the ensembling model has the same number of resamples as the component models.
#' @param x An object of class caretStack
#' @param ... passed to dotplot
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm", trControl = trainControl(method = "cv"))
#' dotplot.caretStack(meta_model)
#' }
dotplot.caretStack <- function(x, ...) {
  resamps <- caret::resamples(x$models)
  lattice::dotplot(resamps, ...)
}
