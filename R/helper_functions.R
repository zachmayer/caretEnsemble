#####################################################
# Configuration Functions
#####################################################
#' @title Return the configured target binary class level
#' @description For binary classification problems, ensemble
#' stacks and certain performance measures require an awareness
#' of which class in a two-factor outcome is the "target" class.
#' By default, this class will be assumed to be the first level in
#' an outcome factor but that setting can be overridden using
#' \code{setBinaryTargetLevel(2L)}.
#' @seealso setBinaryTargetLevel
#' @return Currently configured binary target level (as integer equal to 1 or 2)
#' @export
getBinaryTargetLevel <- function() {
  arg <- getOption("caret.ensemble.binary.target.level", default = 1L)
  validateBinaryTargetLevel(arg)
}

#' @title Set the target binary class level
#' @description For binary classification problems, ensemble
#' stacks and certain performance measures require an awareness
#' of which class in a two-factor outcome is the "target" class.
#' By default, the first level in an outcome factor is used but
#' this value can be overridden using \code{setBinaryTargetLevel(2L)}
#' @param level an integer in \{1, 2\} to be used as target outcome level
#' @seealso getBinaryTargetLevel
#' @export
setBinaryTargetLevel <- function(level) {
  level <- validateBinaryTargetLevel(level)
  options(caret.ensemble.binary.target.level = level)
}

#' @title Validate arguments given as binary target level
#' @description Helper function used to ensure that target
#' binary class levels given by clients can be coerced to an integer
#' and that the resulting integer is in \{1, 2\}.
#' @param arg argument to potentially be used as new target level
#' @return Binary target level (as integer equal to 1 or 2)
validateBinaryTargetLevel <- function(arg) {
  val <- suppressWarnings(try(as.integer(arg), silent = TRUE))
  if (!is.integer(val) || !val %in% c(1L, 2L)) {
    stop(paste0(
      "Specified target binary class level is not valid.  ",
      "Value should be either 1 or 2 but '", arg, "' was given ",
      "(see caretEnsemble::setBinaryTargetLevel for more details)"
    ))
  }
  val
}

#' @title Return the configured multiclass excluded level
#' @description To train a model using probability outputs
#' provided by other models in a classification problem, it is
#' necessary to exclude one of the classes. By default, this class
#' is assumed to be the first level in an outcome factor,
#' but this setting can be overridden using
#' \code{setMulticlassTargetLevel(3L)} if the classification
#' problem has at least 3 classes.
#' @seealso setMulticlassTargetLevel
#' @return Currently configured multiclass excluded level (as integer)
#' @export
getMulticlassExcludedLevel <- function() {
  arg <- getOption("caret.ensemble.multiclass.excluded.level", default = 1L)
  validateMulticlassExcludedLevel(arg)
}

#' @title Set the multiclass excluded level
#' @description To train a model using probability outputs
#' provided by other models in a classification problem, it is
#' necessary to exclude one of the classes. By default, this class
#' is assumed to be the first level in an outcome factor,
#' but this setting can be overridden using
#' \code{setMulticlassTargetLevel(3L)} if the classification
#' problem has at least 3 classes.
#' @note Setting this value outside the range between 1 and
#' the number of classes will cause caretStack to train the model
#' with the probabilities associated with ALL classes, leading to
#' potential collinearity issues.
#' @param level an integer to be used as excluded
#' @seealso getMulticlassExcludedLevel
#' @export
setMulticlassExcludedLevel <- function(level) {
  level <- validateMulticlassExcludedLevel(level)
  options(caret.ensemble.multiclass.excluded.level = level)
}

#' @title Validate arguments given as multiclass excluded level
#' @description Helper function used to ensure that excluded
#' multiclass levels given by clients can be coerced to an integer.
#' @param arg argument to potentially be used as new excluded level
#' @return Multiclass excluded level (as integer)
validateMulticlassExcludedLevel <- function(arg) {
  if (!is.numeric(arg)) {
    stop(paste0(
      "multiclass excluded level must be numeric: ", arg, " was given ",
      "see setMulticlassExcludedLevel for more details"
    ))
  }
  if (!is.finite(arg)) {
    stop(paste0(
      "multiclass excluded level must be finite: ", arg, " was given ",
      "see setMulticlassExcludedLevel for more details"
    ))
  }
  if (arg <= 0) {
    stop(paste0(
      "multiclass excluded level must be > 0: ", arg, " was given ",
      "see setMulticlassExcludedLevel for more details"
    ))
  }
  if (!is.integer(arg)) {
    warning(paste0(
      "multiclass excluded level is not an integer ", arg, " was given ",
      "see setMulticlassExcludedLevel for more details"
    ))
  }
  return(as.integer(arg))
}

#####################################################
# Misc. Functions
#####################################################
#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model predictions
#'
#' @param x a vector of numerics
#' @param w a vector of weights equal to length of x
#' @param na.rm a logical indicating how to handle missing values, default = FALSE
wtd.sd <- function(x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  n <- length(w)
  xWbar <- weighted.mean(x, w, na.rm = na.rm)
  wbar <- mean(w)
  out <- n / ((n - 1) * sum(w)^2) * (sum((w * x - wbar * xWbar)^2) - 2 * xWbar * sum((w - wbar) * (w * x - wbar * xWbar)) + xWbar^2 * sum((w - wbar)^2))
  return(out)
}

#####################################################
# caretList check functions
#####################################################
#' @title Checks caretList model classes
#' @description This function checks caretList classes
#'
#' @param list_of_models a list of caret models to check
check_caretList_classes <- function(list_of_models) {
  # Check that we have a list of train models
  stopifnot(is(list_of_models, "caretList"))
  stopifnot(all(sapply(list_of_models, is, "train")))
  return(invisible(NULL))
}

#' @title Checks that caretList models are all of the same type.
#' @description Validate a caretList
#' @param list_of_models a list of caret models to check
#' @importFrom caret modelLookup
check_caretList_model_types <- function(list_of_models) {
  # Check that models have the same type
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]
  stopifnot(all(types == type)) # TODO: Maybe in the future we can combine reg and class models

  # Check that the model type is VALID
  stopifnot(all(types %in% c("Classification", "Regression")))

  # Warn that we have not yet implemented multiclass models
  # add a check that if this is null you did not set savePredictions in the trainControl
  # TODO: add support for non-prob models (e.g. rFerns)
  if (type == "Classification") {
    for (model in list_of_models) {
      unique_obs <- unique(model$pred$obs)
      if (is.null(unique_obs)) {
        stop("No predictions saved by train. Please re-run models with trainControl set with savePredictions = TRUE.")
      }
    }
  }

  # Check that classification models saved probabilities
  # TODO: ALLOW NON PROB MODELS!
  if (type == "Classification") {
    probModels <- sapply(list_of_models, function(x) is.function(x$modelInfo$prob))
    if (!all(probModels)) stop("All models for classification must be able to generate class probabilities.")
    classProbs <- sapply(list_of_models, function(x) x$control$classProbs)
    if (!all(classProbs)) {
      bad_models <- names(list_of_models)[!classProbs]
      bad_models <- paste(bad_models, collapse = ", ")
      stop("Some models were fit with no class probabilities. Please re-fit them with trainControl, classProbs=TRUE")
    }
  }
  return(invisible(NULL))
}

#' @title Check resamples
#' @description Check that the resamples from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_resamples <- function(modelLibrary) {
  # TODO: ID which model(s) have bad row indexes
  resamples <- lapply(modelLibrary, function(x) x[["Resample"]])
  names(resamples) <- names(modelLibrary)
  check <- length(unique(resamples))
  if (check != 1) {
    stop("Component models do not have the same re-sampling strategies")
  }
  return(invisible(NULL))
}

#' @title Check row indexes
#' @description Check that the row indexes from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_indexes <- function(modelLibrary) {
  # TODO: ID which model(s) have bad row indexes
  rows <- lapply(modelLibrary, function(x) x[["rowIndex"]])
  names(rows) <- names(modelLibrary)
  check <- length(unique(rows))
  if (check != 1) {
    stop("Re-sampled predictions from each component model do not use the same rowIndexes from the origial dataset")
  }
  return(invisible(NULL))
}

#' @title Check observeds
#' @description Check that a list of observed values from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_obs <- function(modelLibrary) {
  # TODO: ID which model(s) have bad row indexes
  obs <- lapply(modelLibrary, function(x) x[["obs"]])
  names(obs) <- names(modelLibrary)
  check <- length(unique(obs))
  if (check != 1) {
    stop("Observed values for each component model are not the same.  Please re-train the models with the same Y variable")
  }
  return(invisible(NULL))
}

#' @title Check predictions
#' @description Check that a list of predictions from a caretList are valid
#'
#' @param modelLibrary a list of predictions from caret models
check_bestpreds_preds <- function(modelLibrary) {
  # TODO: ID which model(s) have bad preds
  # TODO: Regression models should be numeric, classification models should have numeric class probs
  pred <- lapply(modelLibrary, function(x) x[["pred"]])
  names(pred) <- names(modelLibrary)
  classes <- unique(sapply(pred, class))
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
  return(invisible(NULL))
}

#' @title Check multiclass excluded level
#' @description Verifies that the multiclass excluded level is
#' within the range of the number of classes.
#'
#' @param excluded_level the level to exclude
#' @param num_classes the number of classes
check_multiclass_excluded_level <- function(excluded_level, num_classes) {
  if (excluded_level < 1 || excluded_level > num_classes) {
    warning(paste0(
      "The excluded level must be between 1 and the number of classes (",
      num_classes,
      "). ",
      "Provided value was ",
      excluded_level,
      ". ",
      "\nThis value can be changed using setMulticlassExcludedLevel(). ",
      "\nAttempting to train a model with all classes included."
    ))
  }
}

#####################################################
# caretEnsemble check functions
#####################################################
#' @title Check binary classification
#' @description Check that the problem is a binary classification problem
#'
#' @param list_of_models a list of caret models to check
check_binary_classification <- function(list_of_models) {
  if (is.list(list_of_models) && length(list_of_models) > 1) {
    lapply(list_of_models, function(x) {
      if (is(x, "train") &&
        !is.null(x$pred$obs) &&
        is.factor(x$pred$obs) # avoid regression models
      && length(levels(x$pred$obs)) > 2) {
        stop("caretEnsemble only supports binary classification problems")
      }
    })
  }
  return(invisible(NULL))
}

#####################################################
# Extraction functions
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
    validateCustomModel(x$method)$method
  } else if (x$method == "custom") {
    validateCustomModel(x$modelInfo)$method
  } else {
    x$method
  }
}

#' @title Validate a custom caret model info list
#' @description Currently, this only ensures that all model info lists
#' were also assigned a "method" attribute for consistency with usage
#' of non-custom models
#' @param x a model info list (e.g. \code{getModelInfo("rf", regex=F)\[[1]]})
#' @return validated model info list (i.e. x)
validateCustomModel <- function(x) {
  if (is.null(x$method)) {
    stop(paste(
      "Custom models must be defined with a \"method\" attribute containing the name",
      "by which that model should be referenced.  Example: my.glm.model$method <- \"custom_glm\""
    ))
  }
  x
}

#' @title Extracts the model types from a list of train model
#' @description Extracts the model types from a list of train model
#'
#' @param list_of_models an object of class caretList
extractModelTypes <- function(list_of_models) {
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]

  # TODO: Maybe in the future we can combine reg and class models
  # Also, this check is redundant, but I think that"s ok
  stopifnot(all(types == type))
  stopifnot(all(types %in% c("Classification", "Regression")))
  return(type)
}

#' @title Extract the best predictions from a train object
#' @description Extract predictions for the best tune from a model
#' @param x a train object
#' @importFrom data.table data.table setorderv
bestPreds <- function(x) {
  stopifnot(is(x, "train"))
  stopifnot(x$control$savePredictions %in% c("all", "final", TRUE))
  a <- data.table(x$bestTune, key = names(x$bestTune))
  b <- data.table(x$pred, key = names(x$bestTune))
  b <- b[a, ]
  invisible(gc(reset = TRUE))
  setorderv(b, c("Resample", "rowIndex"))
  return(b)
}

#' @title Extract the best predictions from a list of train objects
#' @description Extract predictions for the best tune from a list of caret models
#' @param list_of_models an object of class caretList or a list of caret models
#' @importFrom pbapply pblapply
extractBestPreds <- function(list_of_models) {
  out <- lapply(list_of_models, bestPreds)
  if (is.null(names(out))) {
    names(out) <- make.names(sapply(list_of_models, extractModelName), unique = TRUE)
  }
  invisible(gc(reset = TRUE))
  return(out)
}

#' @title Make a prediction matrix from a list of models
#' @description Extract obs from one models, and a matrix of predictions from all other models, a
#' helper function
#'
#' @param  list_of_models an object of class caretList
#' @importFrom data.table set rbindlist dcast.data.table
makePredObsMatrix <- function(list_of_models) {
  # caretList Checks
  check_caretList_classes(list_of_models)
  check_caretList_model_types(list_of_models)

  # Make a list of models
  modelLibrary <- extractBestPreds(list_of_models)

  # Model library checks
  check_bestpreds_resamples(modelLibrary) # Re-write with data.table?
  check_bestpreds_indexes(modelLibrary) # Re-write with data.table?
  check_bestpreds_obs(modelLibrary) # Re-write with data.table?
  check_bestpreds_preds(modelLibrary) # Re-write with data.table?

  # Extract model type (class or reg)
  type <- extractModelTypes(list_of_models)

  if (type == "Classification") {
    # The names of the columns of the final matrix will consist of a
    # concatenation of the model name and the class name for
    # which the probability is provided.
    # Remove at least one class to avoid colineality problems
    num_classes <- length(levels(list_of_models[[1]]$pred$obs))
    check_multiclass_excluded_level(getMulticlassExcludedLevel(), num_classes)
    if (getMulticlassExcludedLevel() >= 1 && getMulticlassExcludedLevel() <= num_classes) {
      classes_included <- levels(list_of_models[[1]]$pred$obs)[-getMulticlassExcludedLevel()]
    } else {
      classes_included <- levels(list_of_models[[1]]$pred$obs)
    }
    class_model_combinations <- expand.grid(classes_included, names(modelLibrary))
    old_column_names <- apply(class_model_combinations, 1, function(x) paste(x[1], x[2], sep = "_"))
    column_names <- apply(class_model_combinations, 1, function(x) paste(x[2], x[1], sep = "_"))
  } else {
    # Otherwise, just use the model names
    column_names <- names(modelLibrary)
  }

  # Add names column
  for (i in seq_along(modelLibrary)) {
    set(modelLibrary[[i]], j = "modelname", value = names(modelLibrary)[[i]])
  }

  # Remove parameter columns
  keep <- Reduce(intersect, lapply(modelLibrary, names))
  for (i in seq_along(modelLibrary)) {
    rem <- setdiff(names(modelLibrary[[i]]), keep)
    if (length(rem) > 0) {
      for (r in rem) {
        set(modelLibrary[[i]], j = r, value = NULL)
      }
    }
  }
  modelLibrary <- rbindlist(modelLibrary, fill = TRUE)

  # For classification models that produce probs, use the probs as preds
  # Otherwise, just use class predictions
  if (type == "Classification") {
    value.var <- c(levels(modelLibrary$obs), "pred")
  } else {
    value.var <- "pred"
  }

  # Reshape wide for meta-modeling
  modelLibrary <- dcast.data.table(
    modelLibrary,
    obs + rowIndex + Resample ~ modelname,
    value.var = value.var
  )

  if (type == "Classification") {
    # Rename columns asociated with probabilities to modelname_classname
    data.table::setnames(modelLibrary, old = old_column_names, new = column_names)
  }

  return(list(obs = modelLibrary$obs, preds = as.matrix(modelLibrary[, column_names, with = FALSE]), type = type))
}
