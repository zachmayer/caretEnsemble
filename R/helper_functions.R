#####################################################
# Globals
#####################################################

#' @importFrom utils globalVariables
#' @importFrom data.table .SD
#' @importFrom rlang .data
NULL

utils::globalVariables(c(".SD", ".data"))

#####################################################
# Functions for dropping one level from classification problems
#####################################################

#' @title Validate the excluded class
#' @description Helper function to ensure that the excluded level for classification is an integer.  Set to 0L to exclude no class.
#' @param arg The value to check
#' @return integer
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
  if (length(arg) != 1) {
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
      arg <- floor(arg)
    }
    suppressWarnings(out <- as.integer(arg))
  }

  # Check the output
  if (!is.finite(out)) {
    stop(paste0(
      "classification excluded level must be finite: ", arg
    ))
  }
  if (out < 0) {
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
dropExcludedClass <- function(x, all_classes, excluded_class_id) {
  stopifnot(is(x, "data.table"))
  stopifnot(is.character(all_classes))
  excluded_class_id <- validateExcludedClass(excluded_class_id)
  if (length(all_classes) > 1) {
    excluded_class <- all_classes[excluded_class_id] # Note that if excluded_class_id is 0, no class will be excludede
    classes_included <- setdiff(all_classes, excluded_class)
    x <- x[, classes_included, drop = FALSE, with = FALSE]
  }
  x
}

#####################################################
# Extraction functions - train
#####################################################

#' @title Extract the method name associated with a single train object
#' @description Extracts the method name associated with a single train object. Note
#' that for standard models (i.e. those already prespecified by caret), the
#' "method" attribute on the train object is used directly while for custom
#' models the "method" attribute within the model$modelInfo attribute is
#' used instead.
#' @param x a single caret train object
#' @return Name associated with model
extractModelName <- function(x) {
  if (is.list(x$method)) {
    checkCustomModel(x$method)$method
  } else if (x$method == "custom") {
    checkCustomModel(x$modelInfo)$method
  } else {
    x$method
  }
}

#' @title Extract the best predictions from a train object
#' @description Extract predictions for the best tune from a model
#' @param x a train object
#' @importFrom data.table data.table setorderv set
extractBestPreds <- function(x) {
  # Checks
  stopifnot(is(x, "train"))
  stopifnot(x$control$savePredictions %in% c("all", "final", TRUE))

  # Extract the best tune and the pred data
  a <- data.table::data.table(x$bestTune, key = names(x$bestTune))
  b <- data.table::data.table(x$pred, key = names(x$bestTune))

  # Subset pred data to the best tune only
  b <- b[a, ]

  # Remove some columns we don't need and order
  for (var in names(x$bestTune)) {
    data.table::set(b, j = var, value = NULL)
  }
  data.table::setorderv(b, c("Resample", "rowIndex"))

  # Returb
  invisible(gc(reset = TRUE))
  b
}

#####################################################
# Extraction functions - caretList
#####################################################

#' @title Extracts the model type from a list of train models
#' @description Extracts the model types from a list of train models
#'
#' @param list_of_models an object of class caretList
extractModelType <- function(list_of_models) {
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- unique(types)

  # TODO: Maybe in the future we can combine reg and class models
  # Also, this check is redundant, but I think that is ok
  stopifnot(length(type) == 1)
  stopifnot(type %in% c("Classification", "Regression"))
  type
}

#' @title Extract the observed levels from a list of models
#' @description Extract the observed levels from a list of models
#' @param list_of_models an object of class caretList
extractObsLevels <- function(list_of_models) {
  all_levels <- lapply(list_of_models, levels)
  all_levels <- unique(all_levels)
  stopifnot(length(all_levels) == 1)
  all_levels <- all_levels[[1]]
  all_levels
}

#' @title Extract the best predictions (and observeds) from a list of train objects
#' @description Extract predictions (and observeds) for the best tune from a list of caret models. This function extracts
#' the raw preds from regression models and the class probs from classification models.
#' Note that it extract preds and obs in one go, rather than separately. This is because caret can save the internal
#' preds/obs from all resamples rather than just the final.  So we subset the internal pred/obs to just the best tuning
#' (from caret) and return the pred and obs for that tune.
#' @param list_of_models an object of class caretList or a list of caret models
#' @importFrom pbapply pblapply
#' @importFrom data.table set as.data.table
extractBestPredsAndObs <- function(list_of_models, excluded_class_id = 1L) {
  # Determine the type and observed levels
  type <- extractModelType(list_of_models)
  obs_levels <- extractObsLevels(list_of_models)

  # Extraxt best preds based on the tuning information
  preds_and_obs <- lapply(list_of_models, extractBestPreds)

  # Check the extracted data
  check_bestpreds_resamples(preds_and_obs)
  check_bestpreds_indexes(preds_and_obs)
  check_bestpreds_preds(preds_and_obs)
  check_bestpreds_obs(preds_and_obs)

  # Extract the preds
  if (type == "Classification") {
    keep_cols <- extractObsLevels(list_of_models)
  } else if (type == "Regression") {
    keep_cols <- "pred"
  } else {
    stop("Unknown model type")
  }
  preds <- lapply(preds_and_obs, function(x) x[, keep_cols, drop = FALSE, with = FALSE])

  # Drop the excluded level from the preds
  if (type == "Classification") {
    preds <- lapply(preds, dropExcludedClass, all_classes = obs_levels, excluded_class_id = excluded_class_id)
  }

  # Convert list of data.tables into one data.table
  preds <- data.table::as.data.table(preds)

  # Return
  out <- list(
    preds = preds,
    obs = preds_and_obs[[1]][["obs"]],
    rowIndex = preds_and_obs[[1]][["rowIndex"]],
    Resample = preds_and_obs[[1]][["Resample"]],
    type = type
  )
  invisible(gc(reset = TRUE))
  out
}


#####################################################
# check functions
#####################################################

#' @title Validate a custom caret model info list
#' @description Currently, this only ensures that all model info lists
#' were also assigned a "method" attribute for consistency with usage
#' of non-custom models
#' @param x a model info list (e.g. \code{getModelInfo("rf", regex=F)\[[1]]})
#' @return validated model info list (i.e. x)
checkCustomModel <- function(x) {
  if (is.null(x$method)) {
    stop(paste(
      "Custom models must be defined with a \"method\" attribute containing the name",
      "by which that model should be referenced.  Example: my.glm.model$method <- \"custom_glm\""
    ))
  }
  x
}

#' @title Checks caretList model classes
#' @description This function checks caretList classes
#'
#' @param list_of_models a list of caret models to check
check_caretList_classes <- function(list_of_models) {
  # Check that we have a list of train models
  stopifnot(is(list_of_models, "caretList"))
  stopifnot(sapply(list_of_models, is, "train"))
  invisible(NULL)
}

#' @title Checks that caretList models are all of the same type.
#' @description Validate a caretList
#' @param list_of_models a list of caret models to check
#' @importFrom caret modelLookup
check_caretList_model_types <- function(list_of_models) {
  type <- extractModelType(list_of_models)

  # Add a check that class models were trained with probabilities.
  if (type == "Classification") {
    for (model in list_of_models) {
      unique_obs <- unique(model$pred$obs)
      if (is.null(unique_obs)) {
        stop("No predictions saved by train. Please re-run models with trainControl set with savePredictions = 'final'.")
      }
    }
  }

  # Check that classification models saved probabilities
  if (type == "Classification") {
    probModels <- sapply(list_of_models, function(x) is.function(x$modelInfo$prob))
    if (!all(probModels)) stop("All models for classification must be able to generate class probabilities.")
    classProbs <- sapply(list_of_models, function(x) x$control$classProbs)
    if (!all(classProbs)) {
      bad_models <- names(list_of_models)[!classProbs]
      bad_models <- paste(bad_models, collapse = ", ")
      stop("Some models were fit with no class probabilities. Please re-fit them with trainControl, classProbs = TRUE: ", bad_models)
    }
  }
  invisible(NULL)
}

#' @title Check resamples
#' @description Check that the resamples from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_resamples <- function(modelLibrary) {
  resamples <- lapply(modelLibrary, function(x) x[["Resample"]])
  names(resamples) <- names(modelLibrary)
  check <- length(unique(resamples))
  if (check != 1) {
    stop("Component models do not have the same re-sampling strategies")
  }
  invisible(NULL)
}

#' @title Check row indexes
#' @description Check that the row indexes from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_indexes <- function(modelLibrary) {
  rows <- lapply(modelLibrary, function(x) x[["rowIndex"]])
  names(rows) <- names(modelLibrary)
  check <- length(unique(rows))
  if (check != 1) {
    stop("Re-sampled predictions from each component model do not use the same rowIndexes from the origial dataset")
  }
  invisible(NULL)
}

#' @title Check observeds
#' @description Check that a list of observed values from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_obs <- function(modelLibrary) {
  obs <- lapply(modelLibrary, function(x) x[["obs"]])
  names(obs) <- names(modelLibrary)
  check <- length(unique(obs))
  if (check != 1) {
    stop("Observed values for each component model are not the same.  Please re-train the models with the same Y variable")
  }
  invisible(NULL)
}

#' @title Check predictions
#' @description Check that a list of predictions from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_preds <- function(modelLibrary) {
  # TODO: Regression models should be numeric, classification models should have numeric class probs
  pred <- lapply(modelLibrary, function(x) x[["pred"]])
  names(pred) <- names(modelLibrary)

  clases <- sapply(pred, class)
  if (is.matrix(clases)) {
    clases <- apply(clases, 2, paste, collapse = " ")
  }
  classes <- unique(clases)
  check <- length(classes)
  if (check != 1) {
    stop(
      paste0(
        "Component models do not all have the same type of predicitons.  Predictions are a mix of ",
        paste(classes, collapse = ", "),
        "."
      )
    )
  }
  invisible(NULL)
}

#' @title Check binary classification
#' @description Check that the problem is a binary classification problem
#'
#' @param list_of_models a list of caret models to check
#' @export
check_binary_classification <- function(list_of_models) {
  if (is.list(list_of_models) && length(list_of_models) > 1) {
    lapply(list_of_models, function(x) {
      # avoid regression models
      if (is(x, "train") && !is.null(x$pred$obs) && is.factor(x$pred$obs) && length(levels(x$pred$obs)) > 2) {
        stop("caretEnsemble only supports binary classification problems")
      }
    })
  }
  invisible(NULL)
}

#####################################################
# Misc. Functions
#####################################################
#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model predictions
#'
#' @param x a numeric vector
#' @param w a vector of weights equal to length of x
#' @param na.rm a logical indicating how to handle missing values, default = TRUE
#' @export
# https://stats.stackexchange.com/a/61285
wtd.sd <- function(x, w, na.rm = FALSE) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(w))

  xWbar <- weighted.mean(x, w, na.rm = na.rm)
  w <- w / mean(w, na.rm = na.rm)

  variance <- sum((w * (x - xWbar)^2) / (sum(w, na.rm = na.rm) - 1), na.rm = na.rm)
  out <- sqrt(variance)

  out
}
