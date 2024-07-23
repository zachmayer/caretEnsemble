#' @title Generate a specification for fitting a caret model
#' @description A caret model specification consists of 2 parts: a model (as a string) and the arguments to the train call for fitting that model
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
#' @description This function makes sure the tuning parameters passed by the user are valid and have the proper naming, etc.
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
#' @description This function uses modelLookup from caret to ensure the list of methods supplied by the user are all models caret can fit.
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
      data.frame(type = "custom", model = m$method)
    } else if (is.character(m)) {
      data.frame(type = "native", model = m)
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

  if (length(bad_models) > 0) {
    msg <- paste(bad_models, collapse = ", ")
    stop(paste("The following models are not valid caret models:", msg))
  }

  invisible(NULL)
}

#' @title Check that the trainControl object supplied by the user is valid and has defined re-sampling indexes.
#' @description This function checks the user-supplied trainControl object and makes sure it has all the required fields.  If the resampling indexes are missing, it adds them to the model.  If savePredictions=FALSE or "none", this function sets it to "final".
#' @param x a trainControl object.
#' @param y the target for the model.  Used to determine resampling indexes.
#' @importFrom caret createResample createFolds createMultiFolds createDataPartition
#' @return NULL
trControlCheck <- function(x, y) {
  if (!length(x$savePredictions) == 1) {
    stop("Please pass exactly 1 argument to savePredictions, e.g. savePredictions='final'")
  }

  if (!(x$savePredictions %in% c("all", "final"))) {
    warning("trControl$savePredictions not 'all' or 'final'.  Setting to 'final' so we can ensemble the models.")
    x$savePredictions <- "final"
  }

  if (is.null(x$index)) {
    warning("indexes not defined in trControl.  Attempting to set them ourselves, so each model in the ensemble will have the same resampling indexes.")
    if (x$method == "none") {
      stop("Models that aren't resampled cannot be ensembled.  All good ensemble methods rely on out-of sample data.  If you really need to ensemble without re-sampling, try the median or mean of the model's predictions.")
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
        groups = min(5, length(y))
      )
    } else {
      stop(paste0("caretList does not currently know how to handle cross-validation method='", x$method, "'. Please specify trControl$index manually"))
    }
  }
  x
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train model.  Since there are 2 methods to call caret::train, this function also has 2 methods.
#' @param ... a set of arguments, as in the caret::train function
extractCaretTarget <- function(...) {
  UseMethod("extractCaretTarget")
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.default function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.default model.
#' @param x an object where samples are in rows and features are in columns. This could be a simple matrix, data frame or other type (e.g. sparse matrix). See Details below.
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
#' @param ... arguments to pass to \code{\link[caret]{train}}.  These arguments will determine which train method gets dispatched.
#' @param trControl a \code{\link[caret]{trainControl}} object.  We are going to intercept this object check that it has the "index" slot defined, and define the indexes if they are not.
#' @param methodList optional, a character vector of caret models to ensemble.  One of methodList or tuneList must be specified.
#' @param tuneList optional, a NAMED list of caretModelSpec objects. This much more flexible than methodList and allows the specification of model-specific parameters (e.g. passing trace=FALSE to nnet)
#' @param continue_on_fail, logical, should a valid caretList be returned that excludes models that fail, default is FALSE
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
  if (!is.null(methodList) && any(duplicated(methodList))) {
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

  if (length(modelList) == 0) {
    stop("caret:train failed for all models.  Please inspect your data.")
  }
  class(modelList) <- "caretList"

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
  UseMethod("as.caretList")
}

#' @title Convert object to caretList object - For Future Use
#' @description Converts object into a caretList  - For Future Use
#' @param object R object
#' @return NA
#' @export
as.caretList.default <- function(object) {
  # nothing yet, future dreams go here
}

#' @title Convert list to caretList
#' @description Converts list to caretList
#' @param object list of caret models
#' @return a \code{\link{caretList}} object
#' @export
#' @method as.caretList list
as.caretList.list <- function(object) {
  if (!inherits(object, "list")) {
    stop("object must be a list of caret models")
  }
  # Check that each element in the list is of class train
  if (!all(sapply(object, is, "train"))) {
    stop("object requires all elements of list to be caret models")
  }

  class(object) <- "caretList"

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

#' @title Create a matrix of predictions for each of the models in a caretList
#' @description Make a matrix of predictions from a list of caret models
#'
#' @param object an object of class caretList
#' @param newdata New data for predictions.  It can be NULL, but this is ill-advised.
#' @param verbose Logical. If FALSE no progress bar is printed if TRUE a progress
#' bar is shown. Default FALSE.
#' @param excluded_class_id Integer.  The class id to drop when predicting for multiclass
#' @param ... additional arguments to pass to predict.train. Pass the \code{newdata}
#' argument here, DO NOT PASS the "type" argument.  Classification models will
#' return probabilities if possible, and regression models will return "raw".
#' @importFrom pbapply pblapply
#' @importFrom data.table as.data.table setnames
#' @export
#' @method predict caretList
predict.caretList <- function(object, newdata = NULL, verbose = FALSE, excluded_class_id = 1L, ...) {
  # New data check
  if (is.null(newdata)) {
    newdata <- extractTrainingData(object)
  }

  # Decided whether to be verbose or quiet
  apply_fun <- lapply
  if (verbose) {
    apply_fun <- pbapply::pblapply
  }

  # Loop over the models and make predictions
  preds <- apply_fun(object, function(x) {
    type <- x$modelType

    # predict for class
    if (type == "Classification") {
      # use caret::levels.train to extract the levels of the target from each model
      # and then drop the excluded class if needed
      pred <- caret::predict.train(x, type = "prob", newdata = newdata, ... = ...)
      pred <- data.table::as.data.table(pred)
      pred <- dropExcludedClass(pred, all_classes = levels(x), excluded_class_id = excluded_class_id)

      # predict for reg
    } else if (type == "Regression") {
      pred <- caret::predict.train(x, type = "raw", newdata = newdata, ... = ...)
      pred <- data.table::as.data.table(pred)

      # Error
    } else {
      stop(paste("Unknown model type:", type))
    }

    # Return
    pred
  })

  # Name the prediction columns
  if (is.null(names(preds))) {
    # If the model list used for predictions is not currently named,
    # then exctract the model names from each model individually.
    # Note that this should only be possible when caretList objects
    # are created manually
    names(preds) <- make.names(sapply(object, extractModelName), unique = TRUE)
  }

  # Turn a list of data tables into one data.table
  # Note that data.table will name the columns based off the names of the list and the names of each data.table
  preds <- data.table::as.data.table(preds)

  # Return
  preds
}
