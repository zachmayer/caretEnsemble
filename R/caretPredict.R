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
    warning("No excluded_class_id set. Setting to 1L.")
  }
  # Check the input
  if (!is.numeric(arg)) {
    stop(paste0(
      "classification excluded level must be numeric: ", arg
    ))
  }
  if (length(arg) != 1L) {
    stop(paste0(
      "classification excluded level must have a length of 1: length=", length(arg)
    ))
  }

  # Convert to integer if possible
  if (is.integer(arg)) {
    out <- arg
  } else {
    warning(paste0("classification excluded level is not an integer: ", arg))
    if (is.numeric(arg)) {
      out <- floor(arg)
    }
    suppressWarnings(out <- as.integer(out))
  }

  # Check the output
  if (!is.finite(out)) {
    stop(paste0(
      "classification excluded level must be finite: ", arg
    ))
  }
  if (out < 0L) {
    stop(paste0(
      "classification excluded level must be >= 0: ", arg
    ))
  }

  out
}

#' @title Drop Excluded Class
#' @description Drop the excluded class from a prediction data.frame
#' @param x a data.table of predictions
#' @param all_classes a character vector of all classes
#' @param excluded_class_id an integer indicating the class to exclude
#' @keywords internal
dropExcludedClass <- function(x, all_classes, excluded_class_id) {
  stopifnot(is(x, "data.table"), is.character(all_classes))
  excluded_class_id <- validateExcludedClass(excluded_class_id)
  if (length(all_classes) > 1L) {
    excluded_class <- all_classes[excluded_class_id] # Note that if excluded_class_id is 0, no class will be excluded
    classes_included <- setdiff(all_classes, excluded_class)
    x <- x[, classes_included, drop = FALSE, with = FALSE]
  }
  x
}

#' @title Extract the model type from a \code{\link[caret]{train}} object
#' @description Extract the model type from a \code{\link[caret]{train}} object.
#' For classification, validates that the model can predict probabilities, and,
#'  if stacked predictions are requested, that classProbs = TRUE.
#' @param object a \code{\link[caret]{train}} object
#' @param validate_for_stacking a logical indicating whether to validate the object for stacked predictions
#' @return a character string
#' @keywords internal
extractModelType <- function(object, validate_for_stacking = TRUE) {
  stopifnot(is(object, "train"))

  # Must be a train object
  if (!is(object, "train")) {
    stop("object must be a train object")
  }

  # Extract type
  model_type <- object$modelType

  # Class or reg?
  is_class <- model_type == "Classification"

  # Validate for predictions
  if (is_class && !is.function(object$modelInfo$prob)) {
    stop("No probability function found. Re-fit with a method that supports prob.")
  }
  # Validate for stacked predictions
  if (validate_for_stacking) {
    if (!object$control$savePredictions %in% c("all", "final", TRUE)) {
      stop("Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions.")
    }
    if (is_class && !object$control$classProbs) {
      stop("classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.")
    }
  }

  # Return
  model_type
}

#' @title Prediction wrapper for \code{\link[caret]{train}}
#' @description This is a prediction wrapper for \code{\link[caret]{train}} with several features:
#' - If newdata is null, return stacked predictions from the training job, rather than in-sample predictions.
#' - Always returns probabilities for classification models.
#' - Optionally drops one predicted class for classification models.
#' - Always returns a \code{\link[data.table]{data.table}}
#' @param object a \code{\link[caret]{train}} object
#' @param newdata New data to use for predictions.  If NULL, stacked predictions from the training data are returned.
#' @param excluded_class_id an integer indicating the class to exclude.  If 0L, no class is excluded
#' @param ... additional arguments to pass to \code{\link[caret]{predict.train}}, if newdata is not NULL
#' @return a data.table
caretPredict <- function(object, newdata = NULL, excluded_class_id = 1L, ...) {
  stopifnot(is(object, "train"))

  # Extract the model type
  model_type <- extractModelType(object, validate_for_stacking = is.null(newdata))

  # If newdata is NULL, return the stacked predictions
  if (is.null(newdata)) {
    # Extract the best tune
    a <- data.table::data.table(object$bestTune, key = names(object$bestTune))

    # Extract the best predictions
    b <- data.table::data.table(object$pred, key = names(object$bestTune))

    # Subset pred data to the best tune only
    pred <- b[a, ]

    # Keep only the predictions
    keep_cols <- "pred"
    if (model_type == "Classification") {
      keep_cols <- levels(object)
    }
    pred <- pred[, c("rowIndex", keep_cols), drop = FALSE, with = FALSE]

    # If we have multiple resamples per row
    # e.g. for repeated CV, we need to average the predictions
    data.table::setkeyv(pred, "rowIndex")
    pred <- pred[, lapply(.SD, mean), by = "rowIndex"]
    data.table::setorderv(pred, "rowIndex")

    # Remove the rowIndex
    data.table::set(pred, j = "rowIndex", value = NULL)

    # Otherwise, predict on newdata
  } else {
    if (model_type == "Classification") {
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
  if (model_type == "Classification") {
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

  # Return
  pred
}
