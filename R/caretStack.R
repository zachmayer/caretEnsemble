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
#' @param new_y The outcome variable to predict on for the caretList, prior to training the stack
#' (for transfer learning).
#' If NULL, will use the observed levels from the first model in the caret stack
#' If 0, will include all levels.
#' @param metric the metric to use for grid search on the stacking model.
#' @param trControl a trainControl object to use for training the ensemble model. If NULL, will use defaultControl.
#' @param excluded_class_id The integer level to exclude from binary classification or multiclass problems.
#' @param use_original_features a logical indicating whether to include the original features in the stack.
#' If TRUE, will include all the original features used in training the models in the stack. If FALSE, will
#' only include the predictions from the models in the stack.
#' @param which.var a character vector with the names of the original features to include in the stack or a numeric
#' vector with the indexes of the original features to include in the stack. If NULL, will include all original
#' features. If use_original_features is FALSE, this argument is ignored.
#' @param ... additional arguments to pass to the stacking model
#' @return S3 caretStack object
#' @references Caruana, R., Niculescu-Mizil, A., Crew, G., & Ksikes, A. (2004).
#'   Ensemble Selection from Libraries of Models.
#'   \url{https://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
#' @export
#' @examples
#' models <- caretList(
#'   x = iris[1:50, 1:2],
#'   y = iris[1:50, 3],
#'   methodList = c("rpart", "glm")
#' )
#' caretStack(models, method = "glm")
caretStack <- function(
    all.models,
    new_X = NULL,
    new_y = NULL,
    metric = NULL,
    trControl = NULL,
    excluded_class_id = 1L,
    use_original_features = FALSE,
    which.var = NULL,
    ...) {
  # Check all.models
  if (!methods::is(all.models, "caretList")) {
    warning("Attempting to coerce all.models to a caretList.", call. = FALSE)
    all.models <- as.caretList(all.models)
  }

  # Make sure either both or neither new_X and new_y are NULL
  if (is.null(new_X) != is.null(new_y)) {
    stop("Both new_X and new_y must be NULL, or neither.", call. = FALSE)
  }
  if (!is.null(new_X)) {
    stopifnot(
      is.data.frame(new_X) || is.matrix(new_X),
      is.numeric(new_y) || is.factor(new_y) || is.character(new_y),
      nrow(new_X) == length(new_y)
    )
    new_X <- data.table::as.data.table(new_X)
  }

  # Check use_original_features and which.var
  updated_params <- check_use_original_features(use_original_features, new_X, which.var)
  use_original_features <- updated_params$use_original_features
  which.var <- updated_params$which.var

  # Check which.var is a character vector, a numeric vector, or NULL
  # and that all variables are in new_X columns or are valid indexes
  # and convert always to character vector
  which.var <- process_which_var(use_original_features, colnames(new_X), which.var)

  # Validators
  excluded_class_id <- validateExcludedClass(excluded_class_id)

  # Predict for each model. If new_X is NULL, will return stacked predictions
  preds <- predict.caretList(all.models, newdata = new_X, excluded_class_id = excluded_class_id)
  if (!is.null(new_X)) {
    stopifnot(nrow(preds) == nrow(new_X))
  }

  # Add original features to the stack if requested
  if (use_original_features) {
    preds <- cbind(preds, new_X[, which.var, with = FALSE])
  }

  # Choose the target
  obs <- new_y
  if (is.null(obs)) {
    obs <- data.table::data.table(all.models[[1L]]$pred)
    data.table::setorderv(obs, "rowIndex")
    obs <- obs[, list(obs = obs[1L]), by = "rowIndex"]
    obs <- obs[["obs"]]
  }
  stopifnot(nrow(preds) == length(obs))

  # Make a trainControl
  is_class <- is.factor(obs) || is.character(obs)
  is_binary <- length(unique(obs)) == 2L
  if (is.null(metric)) {
    metric <- defaultMetric(is_class = is_class, is_binary = is_binary)
  }
  if (is.null(trControl)) {
    trControl <- defaultControl(obs, is_class = is_class, is_binary = is_binary)
  }

  # Train the model
  model <- caret::train(preds, obs, metric = metric, trControl = trControl, ...)

  # Return final model
  out <- list(
    models = all.models,
    ens_model = model,
    error = model$results,
    excluded_class_id = excluded_class_id,
    original_features_included = which.var
  )
  class(out) <- "caretStack"
  out
}

#' @title Check the parameters use_original_features, new_X, and which.var are consistent
#' @description Check the parameters use_original_features, new_X, and which.var are consistent
#' when training a caretStack.
#' @param use_original_features a logical indicating whether to include the original features in the stack.
#' @param new_X a data.frame of new data to train the stack on.
#' @param which.var a vector indicating which original features to include in the stack.
#' @keywords internal
check_use_original_features <- function(use_original_features, new_X, which.var) {
  # Make sure new_X is not NULL when use_original_features is not NULL
  if (use_original_features && is.null(new_X)) {
    warning("use_original_features is ignored when new_X is NULL.", call. = FALSE)
    use_original_features <- FALSE
    which.var <- NULL
  }

  # Advertise when which.var is ignored
  if (!use_original_features && !is.null(which.var)) {
    warning("which.var is ignored when use_original_features is FALSE.", call. = FALSE)
    which.var <- NULL
  }

  list(use_original_features = use_original_features, which.var = which.var)
}

#' @title Process which.var
#' @description Process the which.var argument for caretStack.
#' @param use_original_features a logical indicating whether to include the original features in the stack.
#' @param feature_colnames a character vector of the names of the original features used to train the stack.
#' @param which.var a character vector of the names of the original features to include in the stack or a numeric
#' vector with the indexes of the original features to include in the stack. If NULL, will include all original
#' features. If use_original_features is FALSE, this argument is ignored.
#' @keywords internal
process_which_var <- function(use_original_features, feature_colnames, which.var) {
  # If use_original_features are not used, which.var is ignored
  if (!use_original_features) {
    return(NULL)
  }
  # Use all original features when which.var is NULL
  if (is.null(which.var)) {
    return(feature_colnames)
  }

  if (is.character(which.var)) {
    # Check that all specified variables in which.var are colnames of
    ignored_vars <- setdiff(which.var, feature_colnames)
    if (length(ignored_vars) > 0L) {
      warning("The following variables are not in new_X and will be ignored: ",
        toString(ignored_vars), ". Ignoring these variables.",
        call. = FALSE
      )
      return(setdiff(which.var, ignored_vars))
    }
    return(which.var)
  }
  if (is.numeric(which.var)) {
    max_index <- length(feature_colnames)
    if (!all(which.var <= max_index)) {
      warning("which.var contains indexes that exceed the number of columns in new_X. Maximum allowed index is ",
        max_index, ". Indexes will be truncated.",
        call. = FALSE
      )
      which.var <- which.var[which.var <= max_index]
    }
    return(feature_colnames[which.var])
  }
  stop(
    "which.var must be a character vector, a numeric vector or NULL ",
    "(to use all the original features).",
    call. = FALSE
  )
}

#' @title Make predictions from a caretStack
#' @description Make predictions from a caretStack. This function passes the data to each function in
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param object a  \code{\link{caretStack}} to make predictions from.
#' @param newdata a new dataframe to make predictions on
#' @param se logical, should prediction errors be produced? Default is false.
#' @param level tolerance/confidence level
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
#' @method predict caretStack
#' @examples
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' RMSE(predict(meta_model, iris[101:150, 1:2]), iris[101:150, 3])
predict.caretStack <- function(
    object,
    newdata = NULL,
    se = FALSE,
    level = 0.95,
    excluded_class_id = 0L,
    return_class_only = FALSE,
    verbose = FALSE,
    ...) {
  # Check the object
  check_caretStack(object)

  # Extract model types
  is_class <- isClassifier(object)

  # If the excluded class wasn't set at train time, set it
  object <- set_excluded_class_id(object, is_class)

  # Check return_class_only
  if (return_class_only) {
    stopifnot(is_class, !se)
    excluded_class_id <- 0L
  }

  # Make sure original features saved in the training data are present in the newdata
  if (!is.null(newdata) && !is.null(object$original_features_included)) {
    check_newdata_features(newdata, object$original_features_included)
  }

  # Get predictions from the submodels on the new data
  # We need theres if there's newdata, for passing the base model predictions to the stack model
  # We also need these if we're calculting standard errors for the predictions
  if (!is.null(newdata) || se) {
    sub_model_preds <- stats::predict(
      object$models,
      newdata = newdata,
      verbose = verbose,
      excluded_class_id = object[["excluded_class_id"]]
    )
    if (!is.null(object$original_features_included)) {
      sub_model_preds <- cbind(sub_model_preds, newdata[, object$original_features_included])
    }
  }

  # Now predict on the stack
  # If newdata is NULL, this will be stacked predictions from caret::train
  # If newdata is present, this will be regular predictions on top
  # of the sub_model_preds.
  meta_preds <- caretPredict(
    object$ens_model,
    newdata = if (!is.null(newdata)) sub_model_preds,
    excluded_class_id = excluded_class_id,
    ...
  )

  # Decide output:
  # IF SE, data.table of predictins, lower, and upper bounds
  # IF return_class_only, factor of class levels
  # ELSE, data.table of predictions
  if (se) {
    imp <- caret::varImp(object, newdata = newdata, normalize = TRUE)
    std_error <- as.matrix(sub_model_preds[, names(imp), with = FALSE])
    std_error <- apply(std_error, 1L, wtd.sd, w = imp, na.rm = TRUE)
    std_error <- stats::qnorm(level) * std_error
    meta_preds <- if (ncol(meta_preds) == 1L) meta_preds[[1L]] else meta_preds
    out <- data.table::data.table(
      pred = meta_preds,
      lwr = meta_preds - std_error,
      upr = meta_preds + std_error
    )
  } else if (return_class_only) {
    # Map to class levels
    class_id <- apply(meta_preds, 1L, which.max)
    class_levels <- levels(object$ens_model)
    out <- factor(class_levels[class_id], class_levels)
  } else {
    out <- meta_preds
  }

  # Return
  out
}

#' @title Check caretStack object
#' @description Make sure a caretStack has both a caretList and a train object
#'
#' @param object a caretStack object
#' @keywords internal
check_caretStack <- function(object) {
  stopifnot(
    methods::is(object, "caretStack"),
    methods::is(object$models, "caretList"),
    methods::is(object$ens_model, "train")
  )
}

#' @title Check original_features_included and newdata
#' @description Make sure newdata contains all the original features used in training
#'
#' @param newdata a data.frame of new data
#' @param original_features_included a character vector of original features used in training
#' @keywords internal
check_newdata_features <- function(newdata, original_features_included) {
  if (!all(original_features_included %in% colnames(newdata))) {
    missing_columns <- setdiff(colnames(newdata), original_features_included)
    stop("newdata does not contain all the original features used in training. Missing columns: ",
      toString(missing_columns),
      call. = FALSE
    )
  }
}

#' @title Set excluded class id
#' @description Set the excluded class id for a caretStack object
#'
#' @param object a caretStack object
#' @param is_class the model type as a logical vector with length 1
#' @keywords internal
set_excluded_class_id <- function(object, is_class) {
  if (is_class && is.null(object[["excluded_class_id"]])) {
    object[["excluded_class_id"]] <- 1L
    warning("No excluded_class_id set. Setting to 1L.", call. = FALSE)
  }
  object
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

#' @title Print a caretStack object
#' @description This is a function to print a caretStack.
#' @param x An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' print(meta_model)
print.caretStack <- function(x, ...) {
  cat("The following models were ensembled:", toString(names(x$models)), " \n")
  cat("\ncaret::train model:\n")
  print(x$ens_model)
  cat("\nFinal model:\n")
  print(x$ens_model$finalModel)
}

#' @title Summarize a caretStack object
#' @description This is a function to summarize a caretStack.
#' @param object An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' summary(meta_model)
summary.caretStack <- function(object, ...) {
  metric <- object$ens_model$metric
  out <- list(
    models = toString(names(object$models)),
    imp = round(caret::varImp(object), 4L),
    metric = metric,
    results = extractMetric(object, metric = metric)
  )
  class(out) <- "summary.caretStack"
  out
}

#' @title Print a summary.caretStack object
#' @description This is a function to print a summary.caretStack.
#' @param x An object of class summary.caretStack
#' @param ... ignored
#' @method print summary.caretStack
#' @export
print.summary.caretStack <- function(x, ...) {
  cat("The following models were ensembled:", x$models, " \n")
  cat("\nModel Importance:\n")
  print(x$imp)
  cat("\nModel accuracy:\n")
  print(x$results)
}

#' @title Variable importance for caretStack
#' @description This is a function to extract variable importance from a caretStack.
#' @param object An object of class caretStack
#' @param newdata the data to use for computing importance. If NULL, will use the stacked predictions from the models.
#' @param normalize a logical indicating whether to normalize the importances to sum to one.
#' @param ... passed to predict.caretList
#' @importFrom caret varImp
#' @method varImp caretStack
#' @export
varImp.caretStack <- function(object, newdata = NULL, normalize = TRUE, ...) {
  # Make sure newdata is not NULL when use_original_features is not NULL
  if (is.null(newdata) && !is.null(object$original_features_included)) {
    stop(
      "Original features were used in training, but they cannot be used if newdata is NULL. ",
      "Please provide newdata to compute variable importance.",
      call. = FALSE
    )
  }
  preds <- predict.caretList(object$models, newdata = newdata, excluded_class_id = object$excluded_class_id, ...)
  if (!is.null(newdata) && !is.null(object$original_features_included)) {
    newdata <- as.data.frame(newdata)
    check_newdata_features(newdata, object$original_features_included)
    preds <- cbind(preds, newdata[, object$original_features_included])
  }
  imp <- permutationImportance(object$ens_model, preds, normalize = normalize)
  imp
}

#' @title Comparison dotplot for a caretStack object
#' @description This is a function to make a dotplot from a caretStack. It uses dotplot from the
#' caret package on all the models in the ensemble, excluding the final ensemble model.At the moment,
#' this function only works if the ensembling model has the same number of resamples as the component models.
#' @param x An object of class caretStack
#' @param ... passed to dotplot
#' @method dotplot caretStack
#' @importFrom lattice dotplot
#' @export
#' @examples
#' set.seed(42)
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' lattice::dotplot(meta_model)
dotplot.caretStack <- function(x, ...) {
  resamps <- caret::resamples(x$models)
  lattice::dotplot(resamps, ...)
}

#' @title Extract accuracy metrics from a \code{\link[caretEnsemble]{caretStack}} object
#' @description Extract the cross-validated accuracy metrics from the ensemble model
#' and individual models in a caretStack.
#' @param x a caretStack object
#' @param ... passed to extractMetric.train and extractMetric.caretList
#' @return A data.table with metrics from the ensemble model and individual models.
#' @export
#' @method extractMetric caretStack
extractMetric.caretStack <- function(x, ...) {
  ensemble_metrics <- extractMetric.train(x$ens_model, ...)
  individual_metrics <- extractMetric.caretList(x$models, ...)

  # Update model_name for ensemble
  data.table::set(ensemble_metrics, j = "model_name", value = "ensemble")

  # Combine metrics
  all_metrics <- rbind(ensemble_metrics, individual_metrics)
  all_metrics
}

#' @title Plot a caretStack object
#' @description This function plots the performance of each model in a caretList object.
#' @param x a caretStack object
#' @param metric which metric to plot. If NULL, will use the default metric used to train the model.
#' @param ... ignored
#' @return a ggplot2 object
#' @method plot caretStack
#' @export
plot.caretStack <- function(x, metric = NULL, ...) {
  dat <- extractMetric(x, metric = metric)
  plt <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data[["model_name"]],
      y = .data[["value"]],
      ymin = .data[["value"]] - .data[["sd"]],
      ymax = .data[["value"]] + .data[["sd"]],
      color = .data[["metric"]]
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Model", y = "Metric Value")
  plt
}

#' @title Extracted stacked residuals for the autoplot
#' @description This function extracts the predictions, observeds, and residuals from a \code{train} object.
#' It uses the object's stacked predictions from cross-validation.
#' @param object a \code{train} object
#' @param show_class_id For classification only: which class level to use for residuals
#' @return a data.table::data.table with predictions, observeds, and residuals
#' @keywords internal
stackedTrainResiduals <- function(object, show_class_id = 2L) {
  stopifnot(methods::is(object, "train"))
  is_class <- isClassifier(object)
  predobs <- extractBestPreds(object)
  rowIndex <- predobs[["rowIndex"]]
  pred <- predobs[["pred"]]
  obs <- predobs[["obs"]]
  if (is_class) {
    show_class <- levels(object)[show_class_id]
    pred <- predobs[[show_class]]
    obs <- as.integer(obs == show_class)
  }
  predobs <- data.table::data.table(
    rowIndex = rowIndex,
    pred = pred,
    obs = obs,
    resid = obs - pred
  )
  predobs
}

#' @title Convenience function for more in-depth diagnostic plots of caretStack objects
#' @description This function provides a more robust series of diagnostic plots
#' for a caretEnsemble object.
#' @param object a \code{caretStack} object
#' @param training_data The data used to train the ensemble. Required if xvars is not NULL
#' Must be in the same row order as when the models were trained.
#' @param xvars a vector of the names of x variables to plot against residuals
#' @param show_class_id For classification only: which class level to show on the plot
#' @param ... ignored
#' @return A grid of diagnostic plots. Top left is the range of the performance
#' metric across each component model along with its standard deviation. Top right
#' is the residuals from the ensembled model plotted against fitted values.
#' Middle left is a bar graph of the weights of the component models. Middle
#' right is the disagreement in the residuals of the component models (unweighted)
#' across the fitted values. Bottom left and bottom right are the plots of the
#' residuals against two random or user specified variables. Note that the ensemble
#' must have been trained with savePredictions = "final", which is required to
#' get residuals from the stack for the plot.
#' @importFrom ggplot2 autoplot
#' @importFrom patchwork plot_layout
#' @method autoplot caretStack
#' @export
#' @examples
#' set.seed(42)
#' data(models.reg)
#' ens <- caretStack(models.reg[1:2], method = "lm")
#' autoplot(ens)
# https://github.com/thomasp85/patchwork/issues/226 — why we need importFrom patchwork plot_layout
autoplot.caretStack <- function(object, training_data = NULL, xvars = NULL, show_class_id = 2L, ...) {
  stopifnot(methods::is(object, "caretStack"))
  ensemble_data <- stackedTrainResiduals(object$ens_model, show_class_id = show_class_id)

  # Performance metrics by model
  g1 <- plot(object) + ggplot2::labs(title = "Metric and SD For Component Models")

  # Residuals vs Fitted
  # Disable the object usage linter in here — it raises false positives for .SD and .data
  g2 <- ggplot2::ggplot(ensemble_data, ggplot2::aes(.data[["pred"]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous("Fitted Values") +
    ggplot2::scale_y_continuous("Residual") +
    ggplot2::labs(title = "Residuals vs Fitted") +
    ggplot2::theme_bw()

  # Model Weights
  imp <- caret::varImp(object[["ens_model"]][["finalModel"]])
  wghtFrame <- data.table::as.data.table(imp)
  data.table::set(wghtFrame, j = "method", value = row.names(imp))
  names(wghtFrame) <- c("weights", "method")
  g3 <- ggplot2::ggplot(wghtFrame, ggplot2::aes(.data[["method"]], .data[["weights"]])) +
    ggplot2::geom_bar(stat = "identity", fill = I("gray50"), color = I("black")) +
    ggplot2::labs(title = "Model Weights", x = "Method", y = "Weights") +
    ggplot2::theme_bw()

  # Disagreement in sub-model residuals
  sub_model_data <- lapply(object$models, stackedTrainResiduals, show_class_id = show_class_id)
  for (model_name in names(sub_model_data)) {
    data.table::set(sub_model_data[[model_name]], j = "model", value = model_name)
  }
  sub_model_data <- data.table::rbindlist(sub_model_data, use.names = TRUE, fill = TRUE)
  sub_model_summary <- sub_model_data[, list(
    ymin = min(.SD[["resid"]]),
    ymax = max(.SD[["resid"]]),
    yavg = stats::median(.SD[["resid"]]),
    yhat = .SD[["pred"]][1L]
  ), by = "rowIndex"]
  g4 <- ggplot2::ggplot(sub_model_summary, ggplot2::aes(
    x = .data[["yhat"]],
    y = .data[["yavg"]]
  )) +
    ggplot2::geom_linerange(alpha = I(0.5), ggplot2::aes(
      ymin = .data[["ymin"]],
      ymax = .data[["ymax"]]
    )) +
    ggplot2::geom_point(size = I(3L), alpha = I(0.8)) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(
      method = "lm", se = FALSE,
      linewidth = I(1.1), color = I("red"), linetype = 2L
    ) +
    ggplot2::labs(
      x = "Fitted Values", y = "Range of Resid.",
      title = "Model Disagreement Across Fitted Values"
    )

  # Residuals vs X variables
  out <- (g1 + g2) / (g3 + g4)
  if (!is.null(training_data)) {
    x_data <- data.table::data.table(training_data)
    if (is.null(xvars)) {
      xvars <- sample(names(x_data), 2L)
    }
    data.table::set(x_data, j = "rowIndex", value = seq_len(nrow(x_data)))
    plotdf <- merge(ensemble_data, x_data, by = "rowIndex")
    g5 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[1L]]], .data[["resid"]])) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::scale_x_continuous(xvars[1L]) +
      ggplot2::scale_y_continuous("Residuals") +
      ggplot2::labs(title = paste0("Residuals Against ", xvars[1L])) +
      ggplot2::theme_bw()
    g6 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[2L]]], .data[["resid"]])) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::scale_x_continuous(xvars[2L]) +
      ggplot2::scale_y_continuous("Residuals") +
      ggplot2::labs(title = paste0("Residuals Against ", xvars[2L])) +
      ggplot2::theme_bw()
    out <- out / (g5 + g6)
  }
  out
}
