#' @title Prediction wrapper for \code{\link[caret]{train}}
#' @description This is a prediction wrapper for \code{\link[caret]{train}} with several features:
#' - If newdata is null, return stacked predictions from the training job, rather than in-sample predictions.
#' - Always returns probabilities for classification models.
#' - Optionally drops one predicted class for classification models.
#' - Always returns a \code{\link[data.table]{data.table}}
#' @param object a \code{\link[caret]{train}} object
#' @param newdata New data to use for predictions. If NULL, stacked predictions from the training data are returned.
#' @param excluded_class_id an integer indicating the class to exclude. If 0L, no class is excluded
#' @param ... additional arguments to pass to \code{\link[caret]{predict.train}}, if newdata is not NULL
#' @return a data.table
#' @keywords internal
caretPredict <- function(object, newdata = NULL, excluded_class_id = 1L, ...) {
  stopifnot(methods::is(object, "train"))

  # Extract the model type
  is_class <- isClassifierAndValidate(object, validate_for_stacking = is.null(newdata))

  # If newdata is NULL, return the stacked predictions
  if (is.null(newdata)) {
    pred <- extractBestPreds(object)
    keep_cols <- if (is_class) levels(object) else "pred"
    pred <- pred[, keep_cols, with = FALSE]

    # Otherwise, predict on newdata
  } else {
    if (is_class) {
      pred <- caret::predict.train(object, type = "prob", newdata = newdata, ...)
    } else {
      pred <- caret::predict.train(object, type = "raw", newdata = newdata, ...)
      stopifnot(
        is.vector(pred),
        is.numeric(pred),
        is.null(dim(pred))
      )
      pred <- unname(pred)
    }
    pred <- data.table::data.table(pred)
  }

  # In both cases (stacked predictions and new predictions), drop the excluded class
  # Make sure in both cases we have consitent column names and column order
  # Drop the excluded class for classificaiton
  stopifnot(nrow(pred) == nrow(newdata))
  if (is_class) {
    stopifnot(
      ncol(pred) == nlevels(object),
      names(pred) == levels(object)
    )
    pred <- dropExcludedClass(pred, all_classes = levels(object), excluded_class_id)
  } else {
    stopifnot(
      ncol(pred) == 1L,
      names(pred) == "pred"
    )
  }

  # Retrun
  pred
}

#' @title Wrapper to train caret models
#' @description This function is a wrapper around the `train` function from the `caret` package.
#' It allows for the passing of local and global arguments to the `train` function.
#' It also allows for the option to continue on fail, and to trim the output model.
#' Trimming the model removes components that are not needed for stacking, to save
#' memory and speed up the stacking process. It also converts preds to a data.table.
#' Its an internal function for use with caretList.
#' @param local_args A list of arguments to pass to the `train` function.
#' @param global_args A list of arguments to pass to the `train` function.
#' @param continue_on_fail A logical indicating whether to continue if the `train` function fails.
#'  If `TRUE`, the function will return `NULL` if the `train` function fails.
#' @param trim A logical indicating whether to trim the output model.
#' If `TRUE`, the function will remove some elements that are not needed from the output model.
#' @return The output of the `train` function.
#' @keywords internal
caretTrain <- function(local_args, global_args, continue_on_fail = FALSE, trim = TRUE) {
  # Combine args
  # I think my handling here is correct (update globals with locals, which allows locals be partial)
  # but it would be nice to have some tests
  model_args <- utils::modifyList(global_args, local_args)

  # Fit
  if (continue_on_fail) {
    model <- tryCatch(do.call(caret::train, model_args), error = function(e) {
      warning(conditionMessage(e), call. = FALSE)
      NULL
    })
  } else {
    model <- do.call(caret::train, model_args)
  }

  # Only save stacked predictions for the best model
  if ("pred" %in% names(model)) {
    model[["pred"]] <- extractBestPreds(model)
  }

  if (trim) {
    # Remove some elements that are not needed from the final model
    if (!is.null(model$modelInfo$trim)) {
      model$finalModel <- model$modelInfo$trim(model$finalModel)
    }

    # Remove some elements that are not needed from the train model
    # note that caret::trim will remove stuff we DO need, such as results, preds, besttune, etc.
    removals <- c("call", "dots", "trainingData", "resampledCM")
    for (i in removals) {
      if (i %in% names(model)) {
        model[[i]] <- NULL
      }
    }

    # Remove some elements that are not needed from the model control (within the train model)
    c_removals <- c("index", "indexOut", "indexFinal")
    for (i in c_removals) {
      if (i %in% names(model[["control"]])) {
        model[["control"]][[i]] <- NULL
      }
    }
  }

  # Return
  model
}

#' @title Aggregate mean or first
#' @description For numeric data take the mean. For character data take the first value.
#' @param x a train object
#' @return a data.table::data.table with predictions
#' @keywords internal
aggregate_mean_or_first <- function(x) {
  if (is.numeric(x)) {
    mean(x)
  } else {
    x[1L]
  }
}

#' @title Extract the best predictions from a train object
#' @description Extract the best predictions from a train object.
#' @param x a train object
#' @return a data.table::data.table with predictions
#' @keywords internal
extractBestPreds <- function(x) {
  stopifnot(methods::is(x, "train"))
  if (is.null(x$pred)) {
    stop("No predictions saved during training. Please set savePredictions = 'final' in trainControl", call. = FALSE)
  }
  stopifnot(methods::is(x$pred, "data.frame"))

  # Extract the best tune
  keys <- names(x$bestTune)
  best_tune <- data.table::data.table(x$bestTune, key = keys)

  # Extract the best predictions
  pred <- data.table::data.table(x$pred, key = keys)

  # Subset pred data to the best tune only
  # Drop rows for other tunes
  pred <- pred[best_tune, ]

  # If we have multiple resamples per row
  # e.g. for repeated CV, we need to average the predictions
  keys <- "rowIndex"
  data.table::setkeyv(pred, keys)
  pred <- pred[, lapply(.SD, aggregate_mean_or_first), by = keys]

  # Order results consistently
  data.table::setorderv(pred, keys)

  # Return
  pred
}

#' @title Validate the excluded class
#' @description Helper function to ensure that the excluded level for classification is an integer.
#' Set to 0L to exclude no class.
#' @param arg The value to check
#' @return integer
#' @keywords internal
validateExcludedClass <- function(arg) {
  # Handle the null case (usually old object where the missing level was not defined)
  if (is.null(arg)) {
    arg <- 1L
    warning("No excluded_class_id set. Setting to 1L.", call. = FALSE)
  }
  # Check the input
  if (!is.numeric(arg)) {
    stop("classification excluded level must be numeric: ", arg, call. = FALSE)
  }
  if (length(arg) != 1L) {
    stop("classification excluded level must have a length of 1: length=", length(arg), call. = FALSE)
  }

  # Convert to integer if possible
  if (is.integer(arg)) {
    out <- arg
  } else {
    warning("classification excluded level is not an integer: ", arg, call. = FALSE)
    if (is.numeric(arg)) {
      out <- floor(arg)
    }
    out <- suppressWarnings(as.integer(out))
  }

  # Check the output
  if (!is.finite(out)) {
    stop("classification excluded level must be finite: ", arg, call. = FALSE)
  }
  if (out < 0L) {
    stop("classification excluded level must be >= 0: ", arg, call. = FALSE)
  }

  out
}

#' @title Drop Excluded Class
#' @description Drop the excluded class from a prediction data.table
#' @param x a data.table of predictions
#' @param all_classes a character vector of all classes
#' @param excluded_class_id an integer indicating the class to exclude
#' @keywords internal
dropExcludedClass <- function(x, all_classes, excluded_class_id) {
  stopifnot(methods::is(x, "data.table"), is.character(all_classes))
  excluded_class_id <- validateExcludedClass(excluded_class_id)
  if (length(all_classes) > 1L) {
    excluded_class <- all_classes[excluded_class_id] # Note that if excluded_class_id is 0, no class will be excluded
    classes_included <- setdiff(all_classes, excluded_class)
    x <- x[, classes_included, drop = FALSE, with = FALSE]
  }
  x
}

#' @title S3 definition for concatenating train objects
#'
#' @description take N objects of class train and concatenate into an object of class caretList for future ensembling
#'
#' @param ... the objects of class train to bind into a caretList
#' @return a \code{\link{caretList}} object
#' @export
#' @examples
#' data(iris)
#' model_lm <- caret::train(Sepal.Length ~ .,
#'   data = iris,
#'   method = "lm"
#' )
#'
#' model_rf <- caret::train(Sepal.Length ~ .,
#'   data = iris,
#'   method = "rf",
#'   tuneLength = 1L
#' )
#'
#' model_list <- c(model_lm, model_rf)
c.train <- function(...) {
  new_model_list <- unlist(lapply(list(...), function(x) {
    if (inherits(x, "caretList")) {
      x
    } else if (inherits(x, "train")) {
      x <- list(x)
      names(x) <- x[[1L]]$method
      x
    } else {
      stop("class of modelList1 must be 'caretList' or 'train'", call. = FALSE)
    }
  }), recursive = FALSE)

  # Make sure names are unique
  names(new_model_list) <- make.names(names(new_model_list), unique = TRUE)

  # reset the class to caretList
  class(new_model_list) <- "caretList"

  new_model_list
}

#' @title Generic function to extract accuracy metrics from various model objects
#' @description A generic function to extract cross-validated accuracy metrics from model objects.
#' @param x An object from which to extract metrics.
#' The specific method will be dispatched based on the class of \code{x}.
#' @param ... Additional arguments passed to the specific methods.
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @seealso \code{\link{extractMetric.train}},
#' \code{\link{extractMetric.caretList}},
#' \code{\link{extractMetric.caretStack}}
extractMetric <- function(x, ...) {
  UseMethod("extractMetric")
}

#' @title Extract accuracy metrics from a \code{\link[caret]{train}} model
#' @description Extract the cross-validated accuracy metrics and their SDs from caret.
#' @param x a train object
#' @param metric a character string representing the metric to extract.
#' @param ... ignored
#' If NULL, uses the metric that was used to train the model.
#' @return A numeric representing the metric desired metric.
#' @method extractMetric train
#' @export
extractMetric.train <- function(x, metric = NULL, ...) {
  if (is.null(metric) || !metric %in% names(x$results)) {
    metric <- x$metric
  }

  results <- data.table::data.table(x$results, key = names(x$bestTune))
  best_tune <- data.table::data.table(x$bestTune, key = names(x$bestTune))

  best_results <- results[best_tune, ]
  value <- best_results[[metric]]
  stdev <- best_results[[paste0(metric, "SD")]]
  if (is.null(stdev)) stdev <- NA_real_

  out <- data.table::data.table(
    model_name = x$method,
    metric = metric,
    value = value,
    sd = stdev
  )
  out
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

#' @title Validate a model type
#' @description Validate the model type from a \code{\link[caret]{train}} object.
#' For classification, validates that the model can predict probabilities, and,
#'  if stacked predictions are requested, that classProbs = TRUE.
#' @param object a \code{\link[caret]{train}} object
#' @param validate_for_stacking a logical indicating whether to validate the object for stacked predictions
#' @return a logical. TRUE if classifier, otherwise FALSE.
#' @keywords internal
isClassifierAndValidate <- function(object, validate_for_stacking = TRUE) {
  stopifnot(methods::is(object, "train"))

  is_class <- isClassifier(object)

  # Validate for predictions
  if (is_class && !is.function(object$modelInfo$prob)) {
    stop("No probability function found. Re-fit with a method that supports prob.", call. = FALSE)
  }
  # Validate for stacked predictions
  if (validate_for_stacking) {
    err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
    if (is.null(object$control$savePredictions)) {
      stop(err, call. = FALSE)
    }
    if (!object$control$savePredictions %in% c("all", "final", TRUE)) {
      stop(err, call. = FALSE)
    }
    if (is_class && !object$control$classProbs) {
      stop("classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.", call. = FALSE)
    }
  }

  # Return
  is_class
}
