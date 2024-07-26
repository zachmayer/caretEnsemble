#' @title Generate a specification for fitting a caret model
#' @description A caret model specification consists of 2 parts: a model (as a string) and
#' the arguments to the train call for fitting that model
#' @param method the modeling method to pass to caret::train
#' @param ... Other arguments that will eventually be passed to caret::train
#' @export
#' @return a list of lists
#' @examples
#' caretModelSpec("rf", tuneLength = 5, preProcess = "ica")
caretModelSpec <- function(method = "rf", ...) {
  out <- c(list(method = method), list(...))
  out
}

#' @title Check that the tuning parameters list supplied by the user is valid
#' @description This function makes sure the tuning parameters passed by the user
#' are valid and have the proper naming, etc.
#' @param x a list of user-supplied tuning parameters and methods
#' @return NULL
#' @export
tuneCheck <- function(x) {
  # Check model methods
  stopifnot(is.list(x))

  model_methods <- lapply(x, function(m) m$method)
  methodCheck(model_methods)
  method_names <- sapply(x, extractModelName)

  # Name models
  if (is.null(names(x))) {
    names(x) <- method_names
  }
  i <- names(x) == ""
  if (any(i)) {
    names(x)[i] <- method_names[i]
  }
  names(x) <- make.names(names(x), unique = TRUE)

  # Check params
  stopifnot(sapply(x, is.list))
  x
}

#' @title Check that the methods supplied by the user are valid caret methods
#' @description This function uses modelLookup from caret to ensure the list of
#' methods supplied by the user are all models caret can fit.
#' @param x a list of user-supplied tuning parameters and methods
#' @importFrom caret modelLookup
#' @return NULL
methodCheck <- function(x) {
  # Fetch list of existing caret models
  supported_models <- unique(modelLookup()$model)

  # Split given model methods based on whether or not they
  # are specified as strings or model info lists (ie custom models)
  models <- lapply(x, function(m) {
    if (is.list(m)) {
      checkCustomModel(m)
      data.frame(type = "custom", model = m$method, stringsAsFactors = FALSE)
    } else if (is.character(m)) {
      data.frame(type = "native", model = m, stringsAsFactors = FALSE)
    } else {
      stop(paste0(
        "Method \"", m, "\" is invalid.  Methods must either be character names ",
        "supported by caret (e.g. \"gbm\") or modelInfo lists ",
        "(e.g. getModelInfo(\"gbm\", regex=F))"
      ))
    }
  })
  models <- do.call(rbind, models) # Could use data.table to be more efficient with lots of models

  # Ensure that all non-custom models are valid
  native_models <- subset(models, get("type") == "native")$model
  bad_models <- setdiff(native_models, supported_models)

  if (length(bad_models) > 0L) {
    msg <- paste(bad_models, collapse = ", ")
    stop(paste("The following models are not valid caret models:", msg))
  }

  invisible(NULL)
}

#' @title Check that the trainControl object supplied by the user is valid and has defined re-sampling indexes.
#' @description This function checks the user-supplied trainControl object and makes
#' sure it has all the required fields.
#' If the resampling indexes are missing, it adds them to the model.
#' If savePredictions=FALSE or "none", this function sets it to "final".
#' @param x a trainControl object.
#' @param y the target for the model.  Used to determine resampling indexes.
#' @importFrom caret createResample createFolds createMultiFolds createDataPartition
#' @return NULL
trControlCheck <- function(x, y) {
  if (length(x$savePredictions) != 1L) {
    stop("Please pass exactly 1 argument to savePredictions, e.g. savePredictions='final'")
  }

  if (!(x$savePredictions %in% c("all", "final"))) {
    warning("trControl$savePredictions not 'all' or 'final'.  Setting to 'final' so we can ensemble the models.")
    x$savePredictions <- "final"
  }

  if (is.null(x$index)) {
    # So each model in the ensemble will have the same resampling indexes
    warning("indexes not defined in trControl.  Attempting to set them ourselves.")
    if (x$method == "none") {
      # All good ensemble methods rely on out-of sample data.
      # If you really need to ensemble without re-sampling, try the median or mean of the model's predictions.
      stop("Models that aren't resampled cannot be ensembled.")
    } else if (x$method == "boot" || x$method == "adaptive_boot") {
      x$index <- createResample(y, times = x$number, list = TRUE)
    } else if (x$method == "cv" || x$method == "adaptive_cv") {
      x$index <- createFolds(y, k = x$number, list = TRUE, returnTrain = TRUE)
    } else if (x$method == "repeatedcv") {
      x$index <- createMultiFolds(y, k = x$number, times = x$repeats)
    } else if (x$method == "LGOCV" || x$method == "adaptive_LGOCV") {
      x$index <- createDataPartition(
        y,
        times = x$number,
        p = 0.5,
        list = TRUE,
        groups = min(5L, length(y))
      )
    } else {
      stop(paste0("caretList can't handle cv method='", x$method, "'. Please specify trControl$index manually"))
    }
  }
  x
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
        stop("No predictions saved by train. Please re-run models with trainControl savePredictions = 'final'")
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
      stop(
        "Some models were fit with no class probabilities. Please re-fit them with trainControl, classProbs = TRUE: ",
        bad_models
      )
    }
  }
  invisible(NULL)
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train model.
#' Since there are 2 methods to call caret::train, this function also has 2 methods.
#' @param ... a set of arguments, as in the caret::train function
extractCaretTarget <- function(...) {
  UseMethod("extractCaretTarget")
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.default function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.default model.
#' @param x an object where samples are in rows and features are in columns. This could be a simple matrix, data frame
#' or other type (e.g. sparse matrix). See Details below.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param ... ignored
#' @method extractCaretTarget default
extractCaretTarget.default <- function(x, y, ...) {
  y
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.formula function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.formula model.
#' @param form A formula of the form y ~ x1 + x2 + ...
#' @param data Data frame from which variables specified in formula are preferentially to be taken.
#' @param ... ignored
#' @method extractCaretTarget formula
extractCaretTarget.formula <- function(form, data, ...) {
  y <- model.response(model.frame(form, data))
  names(y) <- NULL
  y
}

#' Create a list of several train models from the caret package
#'
#' Build a list of train objects suitable for ensembling using the \code{\link{caretEnsemble}}
#' function.
#'
#' @param ... arguments to pass to \code{\link[caret]{train}}.
#' These arguments will determine which train method gets dispatched.
#' @param trControl a \code{\link[caret]{trainControl}} object.
#' We are going to intercept this object check that it has the
#' "index" slot defined, and define the indexes if they are not.
#' @param methodList optional, a character vector of caret models to ensemble.
#' One of methodList or tuneList must be specified.
#' @param tuneList optional, a NAMED list of caretModelSpec objects.
#' This much more flexible than methodList and allows the
#' specification of model-specific parameters (e.g. passing trace=FALSE to nnet)
#' @param continue_on_fail, logical, should a valid caretList be returned that
#' excludes models that fail, default is FALSE
#' @return A list of \code{\link[caret]{train}} objects. If the model fails to build,
#' it is dropped from the list.
#' @importFrom caret trainControl train
#' @export
#' @examples
#' \dontrun{
#' myControl <- trainControl(method = "cv", number = 5)
#' caretList(
#'   Sepal.Length ~ Sepal.Width,
#'   head(iris, 50),
#'   methodList = c("glm", "lm"),
#'   trControl = myControl
#' )
#' caretList(
#'   Sepal.Length ~ Sepal.Width,
#'   head(iris, 50),
#'   methodList = c("lm"),
#'   tuneList = list(
#'     nnet = caretModelSpec(method = "nnet", trace = FALSE, tuneLength = 1)
#'   ),
#'   trControl = myControl
#' )
#' }
caretList <- function(
    ...,
    trControl = NULL,
    methodList = NULL,
    tuneList = NULL,
    continue_on_fail = FALSE) {
  # Checks
  if (is.null(trControl)) {
    trControl <- trainControl()
  }
  if (is.null(tuneList) && is.null(methodList)) {
    stop("Please either define a methodList or tuneList")
  }
  if (!is.null(methodList) && anyDuplicated(methodList) > 0L) {
    warning("Duplicate entries in methodList.  Using unqiue methodList values.")
    methodList <- unique(methodList)
  }

  # Make methodList into a tuneList and add onto tuneList
  if (!is.null(methodList)) {
    tuneList <- c(tuneList, lapply(methodList, caretModelSpec))
  }

  # Make sure tuneList is valid
  tuneList <- tuneCheck(tuneList)

  # Capture global arguments for train as a list
  global_args <- list(...)

  # Add indexes to trControl if they are missing
  if (is.null(trControl$index)) {
    target <- extractCaretTarget(...)
    trControl <- trControlCheck(x = trControl, y = target)
  }

  # Squish trControl back onto the global arguments list
  global_args[["trControl"]] <- trControl

  # Loop through the tuneLists and fit caret models with those specs
  modelList <- lapply(tuneList, function(m) {
    model_args <- c(global_args, m)
    if (continue_on_fail) {
      model <- tryCatch(do.call(train, model_args), error = function(e) NULL)
    } else {
      model <- do.call(train, model_args)
    }
    model
  })
  names(modelList) <- names(tuneList)
  nulls <- sapply(modelList, is.null)
  modelList <- modelList[!nulls]

  if (length(modelList) == 0L) {
    stop("caret:train failed for all models.  Please inspect your data.")
  }
  class(modelList) <- c("caretList", "list")

  modelList
}

#' @title Check if an object is a caretList object
#' @description Check if an object is a caretList object
#' @param object an R object
#' @export
is.caretList <- function(object) {
  is(object, "caretList")
}

#' @title Convert object to caretList object
#' @description Converts object into a caretList
#' @param object R Object
#' @return a \code{\link{caretList}} object
#' @export
as.caretList <- function(object) {
  if (is.null(object)) {
    stop("object is null")
  }
  UseMethod("as.caretList", object)
}

#' @title Convert object to caretList object - For Future Use
#' @description Converts object into a caretList  - For Future Use
#' @param object R object
#' @return NA
#' @export
as.caretList.default <- function(object) {
  stop("object must be a list")
}

#' @title Convert list to caretList
#' @description Converts list to caretList
#' @param object list of caret models
#' @return a \code{\link{caretList}} object
#' @export
#' @method as.caretList list
as.caretList.list <- function(object) {
  # Check that the object is a list
  if (!inherits(object, "list")) {
    stop("object must be a list of caret models")
  }

  # Check that each element in the list is of class train
  if (!all(sapply(object, is, "train"))) {
    stop("object requires all elements of list to be caret models")
  }

  # Make sure the class is named
  if (is.null(names(object))) {
    # If the model list used for predictions is not currently named,
    # then exctract the model names from each model individually.
    names(object) <- sapply(object, extractModelName)
  }

  # Make sure the names are valid
  names(object) <- make.names(names(object), unique = TRUE, allow_ = TRUE)

  # Apply the class
  class(object) <- "caretList"

  # Checks
  check_caretList_classes(out)
  check_caretList_model_types(out)
  stop('fuck')
  out

  # Return
  object
}

#' @title Index a caretList
#' @description Index a caret list to extract caret models into a new caretList object
#' @param object an object of class caretList
#' @param index selected index
#' @export
`[.caretList` <- function(object, index) {
  newObj <- `[.listof`(object, index)
  newObj
}

#' @title Validate the excluded class
#' @description Helper function to ensure that the excluded level for classification is an integer.
#' Set to 0L to exclude no class.
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
dropExcludedClass <- function(x, all_classes, excluded_class_id) {
  stopifnot(is(x, "data.table"), is.character(all_classes))
  excluded_class_id <- validateExcludedClass(excluded_class_id)
  if (length(all_classes) > 1L) {
    excluded_class <- all_classes[excluded_class_id] # Note that if excluded_class_id is 0, no class will be excludede
    classes_included <- setdiff(all_classes, excluded_class)
    x <- x[, classes_included, drop = FALSE, with = FALSE]
  }
  x
}

#' @title Create a matrix of predictions for each of the models in a caretList
#' @description Make a matrix of predictions from a list of caret models
#'
#' @param object an object of class caretList
#' @param newdata New data for predictions.  It can be NULL, but this is ill-advised.
#' @param verbose Logical. If FALSE no progress bar is printed if TRUE a progress
#' bar is shown. Default FALSE.
#' @param excluded_class_id Integer. The class id to drop when predicting for multiclass
#' @param ... Other arguments to pass to \code{\link[caret]{predict.train}}
#' @importFrom pbapply pblapply
#' @importFrom data.table as.data.table setnames
#' @export
#' @method predict caretList
predict.caretList <- function(object, newdata = NULL, verbose = FALSE, excluded_class_id = 0L, ...) {
  # Decided whether to be verbose or quiet
  apply_fun <- lapply
  if (verbose) {
    apply_fun <- pbapply::pblapply
  }

  # Check data
  if (is.null(newdata)) {
    train_data_nulls <- sapply((object), function(x) is.null(x[["trainingData"]]))
    if (any(train_data_nulls)) {
      stop("newdata is NULL and trainingData is NULL for some models. Use newdata or retrain with returnData=TRUE.")
    }
  }

  # Loop over the models and make predictions
  preds <- apply_fun(object, function(x) {
    type <- x$modelType

    # predict for class
    if (type == "Classification") {
      # use caret::levels.train to extract the levels of the target from each model
      # and then drop the excluded class if needed
      pred <- caret::predict.train(x, type = "prob", newdata = newdata, ...)
      pred <- data.table::as.data.table(pred)
      pred <- dropExcludedClass(pred, all_classes = levels(x), excluded_class_id = excluded_class_id)

      # predict for reg
    } else if (type == "Regression") {
      pred <- caret::predict.train(x, type = "raw", newdata = newdata)
      pred <- data.table::as.data.table(pred)

      # Error
    } else {
      stop(paste("Unknown model type:", type))
    }

    # Return
    pred
  })

  # Turn a list of data tables into one data.table
  # Note that data.table will name the columns based off the names of the list and the names of each data.table
  preds <- data.table::as.data.table(preds)

  # Return
  preds
}


#' @title Extract the observed levels from a list of models
#' @description Extract the observed levels from a list of models
#' @param list_of_models an object of class caretList
extractObsLevels <- function(list_of_models) {
  all_levels <- lapply(list_of_models, levels)
  all_levels <- unique(all_levels)
  stopifnot(length(all_levels) == 1L)
  all_levels <- all_levels[[1L]]
  all_levels
}

#' @title Extract the best predictions (and observeds) from a list of train objects
#' @description Extract predictions (and observeds) for the best tune from a list of caret models.
#' This function extracts the raw preds from regression models and the class probs from classification models.
#' Note that it extract preds and obs in one go, rather than separately. This is because caret can save the internal
#' preds/obs from all resamples rather than just the final.  So we subset the internal pred/obs to just the best tuning
#' (from caret) and return the pred and obs for that tune.
#' @param list_of_models an object of class caretList or a list of caret models
#' @param excluded_class_id an integer indicating the class to exclude for classification models
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
  }
  preds <- lapply(preds_and_obs, function(x) x[, keep_cols, drop = FALSE, with = FALSE])

  # Drop the excluded level from the preds
  if (type == "Classification") {
    preds <- lapply(preds, dropExcludedClass, all_classes = obs_levels, excluded_class_id = excluded_class_id)
  }

  # Convert list of data.tables into one data.table
  preds <- data.table::as.data.table(preds)

  # Return
  # TODO:
  # - make this a data.table
  # - make Classifciaiton pull from each sub-model
  # - aggregate by row index and sort by row inde3x
  # - merge with all possible IDs, warn on NAs and fill with 0
  # - allow different models, different methods, different resamples, different types.
  # - Only require a common set of rows
  out <- list(
    preds = preds,
    obs = preds_and_obs[[1L]][["obs"]],
    rowIndex = preds_and_obs[[1L]][["rowIndex"]],
    Resample = preds_and_obs[[1L]][["Resample"]],
    type = type
  )
  invisible(gc(reset = TRUE))
  out
}

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