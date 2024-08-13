table #' Create a list of several train models from the caret package
#'
#' Build a list of train objects suitable for ensembling using the \code{\link{caretStack}}
#' function.
#'
#' @param ... arguments to pass to \code{\link[caret]{train}}. Don't use the formula interface, its slower
#' and buggier compared to the X, y interface. Use a \code{\link[data.table]{data.table}} for X.
#' Particularly if you have a large dataset and/or many models, using a data.table will
#' avoid unnecessary copies of your data and can save a lot of time and RAM.
#' These arguments will determine which train method gets dispatched.
#' @param trControl a \code{\link[caret]{trainControl}} object. If NULL, will use defaultControl.
#' @param methodList optional, a character vector of caret models to ensemble.
#' One of methodList or tuneList must be specified.
#' @param tuneList optional, a NAMED list of caretModelSpec objects.
#' This much more flexible than methodList and allows the
#' specification of model-specific parameters (e.g. passing trace=FALSE to nnet)
#' @param metric a string, the metric to optimize for. If NULL, we will choose a good one.
#' @param continue_on_fail logical, should a valid caretList be returned that
#' excludes models that fail, default is FALSE
#' @param trim logical should the train models be trimmed to save memory and speed up stacking
#' @return A list of \code{\link[caret]{train}} objects. If the model fails to build,
#' it is dropped from the list.
#' @export
#' @examples
#' caretList(
#'   Sepal.Length ~ Sepal.Width,
#'   head(iris, 50),
#'   methodList = c("glm", "lm"),
#'   tuneList = list(
#'     nnet = caretModelSpec(method = "nnet", trace = FALSE, tuneLength = 1)
#'   )
#' )
caretList <- function(
    ...,
    trControl = NULL,
    methodList = NULL,
    tuneList = NULL,
    metric = NULL,
    continue_on_fail = FALSE,
    trim = TRUE) {
  # Checks
  if (is.null(tuneList) && is.null(methodList)) {
    stop("Please either define a methodList or tuneList", call. = FALSE)
  }
  if (!is.null(methodList) && anyDuplicated(methodList) > 0L) {
    warning("Duplicate entries in methodList. Using unique methodList values.", call. = FALSE)
    methodList <- unique(methodList)
  }

  # Make methodList into a tuneList and add onto tuneList
  if (!is.null(methodList)) {
    tuneList <- c(tuneList, lapply(methodList, caretModelSpec))
  }

  # Make sure tuneList is valid
  tuneList <- tuneCheck(tuneList)

  # Determine class vs reg
  target <- extractCaretTarget(...)
  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  # Determine metric and trControl
  if (is.null(metric)) {
    metric <- defaultMetric(is_class = is_class, is_binary = is_binary)
  }
  if (is.null(trControl)) {
    trControl <- defaultControl(target, is_class = is_class, is_binary = is_binary)
  }

  # ALWAYS save class probs
  trControl[["classProbs"]] <- is_class
  trControl["savePredictions"] <- "final"

  # Capture global arguments for train as a list
  # Squish trControl back onto the global arguments list
  global_args <- list(...)
  global_args[["trControl"]] <- trControl
  global_args[["metric"]] <- metric

  # Loop through the tuneLists and fit caret models with those specs
  modelList <- lapply(tuneList, caretTrain, global_args = global_args, continue_on_fail = continue_on_fail, trim = trim)
  names(modelList) <- names(tuneList)
  nulls <- vapply(modelList, is.null, logical(1L))
  modelList <- modelList[!nulls]

  if (length(modelList) == 0L) {
    stop("caret:train failed for all models. Please inspect your data.", call. = FALSE)
  }
  class(modelList) <- c("caretList", "list")

  modelList
}

#' @title Create a matrix of predictions for each of the models in a caretList
#' @description Make a matrix of predictions from a list of caret models
#'
#' @param object an object of class caretList
#' @param newdata New data for predictions. It can be NULL, but this is ill-advised.
#' @param verbose Logical. If FALSE no progress bar is printed if TRUE a progress
#' bar is shown. Default FALSE.
#' @param excluded_class_id Integer. The class id to drop when predicting for multiclass
#' @param ... Other arguments to pass to \code{\link[caret]{predict.train}}
#' @method predict caretList
#' @export
predict.caretList <- function(object, newdata = NULL, verbose = FALSE, excluded_class_id = 1L, ...) {
  stopifnot(methods::is(object, "caretList"))

  # Decided whether to be verbose or quiet
  apply_fun <- lapply
  if (verbose) {
    apply_fun <- pbapply::pblapply
  }

  # Loop over the models and make predictions
  if (!is.null(newdata)) {
    newdata <- data.table::as.data.table(newdata)
  }
  preds <- apply_fun(object, caretPredict, newdata = newdata, excluded_class_id = excluded_class_id, ...)
  stopifnot(
    is.list(preds),
    length(preds) >= 1L,
    length(preds) == length(object),
    vapply(preds, data.table::is.data.table, logical(1L))
  )

  # All preds must have the same number of rows.
  # We allow different columns, and even different column names!
  # E.g. you could mix classification and regression models
  # caretPredict will aggregate multiple predictions for the same row (e.g. repeated CV)
  # caretPredict will make sure the rows are sorted by the original row order
  # If you want to ensemble models that were trained on different rows of data, use
  # newdata to predict on a common dataset so they can be ensembles.
  pred_rows <- vapply(preds, nrow, integer(1L))
  stopifnot(pred_rows == pred_rows[1L])

  # Name the predictions
  for (i in seq_along(preds)) {
    p <- preds[[i]]
    model_name <- names(object)[i]
    if (ncol(p) == 1L) {
      # For a single column, name it after the model (e.g. regression or binary with an excluded class)
      new_names <- model_name
    } else {
      # For multiple columns, name them including the model (e.g. multiclass)
      new_names <- paste(model_name, names(p), sep = "_")
    }
    data.table::setnames(p, names(p), new_names)
  }
  preds <- unname(preds)

  # Combine the predictions into a single data.table
  preds <- data.table::as.data.table(preds)

  stopifnot(
    !is.null(names(preds)),
    length(dim(preds)) == 2L
  )
  all_regression <- all(vapply(object, function(x) x$modelType == "Regression", logical(1L)))
  if (all_regression) {
    stopifnot(
      length(names(preds)) == length(object),
      names(preds) == names(object)
    )
  }

  # Return
  preds
}

#' @title Construct a default train control for use with caretList
#' @description Unlike caret::trainControl, this function defaults to 5 fold CV.
#' CV is good for stacking, as every observation is in the test set exactly once.
#' We use 5 instead of 10 to save compute time, as caretList is for fitting many
#' models. We also construct explicit fold indexes and return the stacked predictions,
#' which are needed for stacking. For classification models we return class probabilities.
#' @param target the target variable.
#' @param method the method to use for trainControl.
#' @param number the number of folds to use.
#' @param savePredictions the type of predictions to save.
#' @param index the fold indexes to use.
#' @param is_class logical, is this a classification or regression problem.
#' @param is_binary logical, is this binary classification.
#' @param ... other arguments to pass to \code{\link[caret]{trainControl}}
#' @export
defaultControl <- function(
    target,
    method = "cv",
    number = 5L,
    savePredictions = "final",
    index = caret::createFolds(target, k = number, list = TRUE, returnTrain = TRUE),
    is_class = is.factor(target) || is.character(target),
    is_binary = length(unique(target)) == 2L,
    ...) {
  stopifnot(savePredictions %in% c("final", "all"))
  caret::trainControl(
    method = method,
    number = number,
    index = index,
    savePredictions = savePredictions,
    classProbs = is_class,
    summaryFunction = ifelse(is_class && is_binary, caret::twoClassSummary, caret::defaultSummary),
    returnData = FALSE,
    ...
  )
}

#' @title Construct a default metric
#' @description Caret defaults to RMSE for classification and RMSE for regression.
#' For classification, I would rather use ROC.
#' @param is_class logical, is this a classification or regression problem.
#' @param is_binary logical, is this binary classification.
#' @export
defaultMetric <- function(is_class, is_binary) {
  if (is_class) {
    if (is_binary) {
      "ROC"
    } else {
      "Accuracy"
    }
  } else {
    "RMSE"
  }
}

#' @title Convert object to caretList object
#' @description Converts object into a caretList
#' @param object R Object
#' @return a \code{\link{caretList}} object
#' @export
as.caretList <- function(object) {
  if (is.null(object)) {
    stop("object is null", call. = FALSE)
  }
  UseMethod("as.caretList", object)
}

#' @title Convert object to caretList object - For Future Use
#' @description Converts object into a caretList  - For Future Use
#' @param object R object
#' @return NA
#' @export
as.caretList.default <- function(object) {
  stop("object must be a list", call. = FALSE)
}

#' @title Convert list to caretList
#' @description Converts list to caretList
#' @param object list of caret models
#' @return a \code{\link{caretList}} object
#' @export
as.caretList.list <- function(object) {
  # Check that the object is a list
  if (!inherits(object, "list")) {
    stop("object must be a list of caret models", call. = FALSE)
  }

  # Check that each element in the list is of class train
  if (!all(vapply(object, methods::is, logical(1L), "train"))) {
    stop("object requires all elements of list to be caret models", call. = FALSE)
  }

  # Make sure the class is named
  if (is.null(names(object))) {
    # If the model list used for predictions is not currently named,
    # then exctract the model names from each model individually.
    names(object) <- vapply(object, extractModelName, character(1L))
  }

  # Make sure the names are valid
  names(object) <- make.names(names(object), unique = TRUE, allow_ = TRUE)

  # Apply the class
  class(object) <- "caretList"

  # Return
  object
}

#' @title S3 definition for concatenating caretList
#'
#' @description take N objects of class caretList and concatenate them into a larger object of
#' class caretList for future ensembling
#'
#' @param ... the objects of class caretList or train to bind into a caretList
#' @return a \code{\link{caretList}} object
#' @export
#' @examples
#' data(iris)
#' model_list1 <- caretList(Sepal.Width ~ .,
#'   data = iris,
#'   tuneList = list(
#'     lm = caretModelSpec(method = "lm")
#'   )
#' )
#'
#' model_list2 <- caretList(Sepal.Width ~ .,
#'   data = iris, tuneLength = 1L,
#'   tuneList = list(
#'     rf = caretModelSpec(method = "rf")
#'   )
#' )
#'
#' bigList <- c(model_list1, model_list2)
c.caretList <- function(...) {
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

#' @title Index a caretList
#' @description Index a caret list to extract caret models into a new caretList object
#' @param object an object of class caretList
#' @param index selected index
#' @export
`[.caretList` <- function(object, index) {
  newObj <- `[.listof`(object, index)
  newObj
}

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
  out <- list(method = method, ...)
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
  method_names <- vapply(x, extractModelName, character(1L))

  # Name models
  if (is.null(names(x))) {
    names(x) <- method_names
  }
  i <- !nzchar(names(x))
  if (any(i)) {
    names(x)[i] <- method_names[i]
  }
  names(x) <- make.names(names(x), unique = TRUE)

  # Check params
  stopifnot(vapply(x, is.list, logical(1L)))
  x
}

#' @title Validate a custom caret model info list
#' @description Currently, this only ensures that all model info lists
#' were also assigned a "method" attribute for consistency with usage
#' of non-custom models
#' @param x a model info list (e.g. \code{getModelInfo("rf", regex=F)\[[1]]})
#' @return validated model info list (i.e. x)
#' @keywords internal
checkCustomModel <- function(x) {
  if (is.null(x$method)) {
    stop(
      "Custom models must be defined with a \"method\" attribute containing the name",
      "by which that model should be referenced. Example: my.glm.model$method <- \"custom_glm\"",
      call. = FALSE
    )
  }
  x
}

#' @title Check that the methods supplied by the user are valid caret methods
#' @description This function uses modelLookup from caret to ensure the list of
#' methods supplied by the user are all models caret can fit.
#' @param x a list of user-supplied tuning parameters and methods
#' @return NULL
#' @keywords internal
methodCheck <- function(x) {
  # Fetch list of existing caret models
  supported_models <- unique(caret::modelLookup()$model)

  # Split given model methods based on whether or not they
  # are specified as strings or model info lists (ie custom models)
  models <- lapply(x, function(m) {
    if (is.list(m)) {
      checkCustomModel(m)
      data.table::data.table(type = "custom", model = m$method, stringsAsFactors = FALSE)
    } else if (is.character(m)) {
      data.table::data.table(type = "native", model = m, stringsAsFactors = FALSE)
    } else {
      stop(
        "Method \"", m, "\" is invalid. Methods must either be character names ",
        "supported by caret (e.g. \"gbm\") or modelInfo lists ",
        "(e.g. getModelInfo(\"gbm\", regex=F))",
        call. = FALSE
      )
    }
  })
  models <- data.table::rbindlist(models, use.names = TRUE, fill = TRUE)

  # Ensure that all non-custom models are valid
  native_models <- subset(models, get("type") == "native")$model
  bad_models <- setdiff(native_models, supported_models)

  if (length(bad_models) > 0L) {
    msg <- toString(bad_models)
    stop("The following models are not valid caret models: ", msg, call. = FALSE)
  }

  invisible(NULL)
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train model.
#' Since there are 2 methods to call caret::train, this function also has 2 methods.
#' @param ... a set of arguments, as in the caret::train function
#' @keywords internal
extractCaretTarget <- function(...) {
  UseMethod("extractCaretTarget")
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.default function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.default model.
#' @param x an object where samples are in rows and features are in columns. This could be a simple matrix, data frame
#' or other type (e.g. sparse matrix). See Details below.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param ... ignored
#' @keywords internal
extractCaretTarget.default <- function(x, y, ...) {
  y
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.formula function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.formula model.
#' @param form A formula of the form y ~ x1 + x2 + ...
#' @param data Data frame from which variables specified in formula are preferentially to be taken.
#' @param ... ignored
#' @method extractCaretTarget formula
#' @keywords internal
extractCaretTarget.formula <- function(form, data, ...) {
  y <- stats::model.response(stats::model.frame(form, data))
  names(y) <- NULL
  y
}

#' @title Extract accuracy metrics from a \code{\link[caretEnsemble]{caretList}} object
#' @description Extract the cross-validated accuracy metrics from each model in a caretList.
#' @param x a caretList object
#' @param ... passed to extractMetric.train
#' @return A data.table with metrics from each model.
#' @method extractMetric caretList
#' @export
extractMetric.caretList <- function(x, ...) {
  metrics <- lapply(x, extractMetric.train, ...)
  metrics <- data.table::rbindlist(metrics, use.names = TRUE, fill = TRUE)
  metrics
}

#' @title Plot a caretList object
#' @description This function plots the performance of each model in a caretList object.
#' @param x a caretList object
#' @param metric which metric to plot
#' @param ... ignored
#' @return A ggplot2 object
#' @method plot caretList
#' @export
plot.caretList <- function(x, metric = NULL, ...) {
  dat <- extractMetric(x, metric = metric)
  plt <- ggplot2::ggplot(
    dat, ggplot2::aes(
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

#' @title Summarize a caretList
#' @description This function summarizes the performance of each model in a caretList object.
#' @param object a caretList object
#' @param metric The metric to show. If NULL will use the metric used to train each model
#' @param ... passed to extractMetric
#' @return A data.table with metrics from each model.
#' @method summary caretList
#' @export
summary.caretList <- function(object, metric = NULL, ...) {
  out <- list(
    models = toString(names(object)),
    results = extractMetric(object, metric = metric)
  )
  class(out) <- "summary.caretList"
  out
}

#' @title Print a summary.caretList object
#' @description This is a function to print a summary.caretList
#' @param x An object of class summary.caretList
#' @param ... ignored
#' @method print summary.caretList
#' @export
print.summary.caretList <- function(x, ...) {
  cat("The following models were ensembled:", x$models, " \n")
  cat("\nModel accuracy:\n")
  print(x$results)
}
