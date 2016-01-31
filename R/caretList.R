#' @title Generate a specification for fitting a caret model
#' @description A caret model specificaiton consists of 2 parts: a model (as a string) and the argments to the train call for fitting that model
#' @param method the modeling method to pass to caret::train
#' @param ... Other arguments that will eventually be passed to caret::train
#' @export
#' @return a list of lists
#' @examples
#' caretModelSpec("rf", tuneLength=5, preProcess="ica")
caretModelSpec <- function(method="rf", ...){
  stopifnot(is.character(method))
  out <- c(list(method=method), list(...))
  return(out)
}

#' @title Check that the tuning parameters list supplied by the user is valid
#' @description This function makes sure the tuning parameters passed by the user are valid and have the proper naming, etc.
#' @param x a list of user-supplied tuning parameters and methods
#' @return NULL
tuneCheck <- function(x){

  #Check model methods
  stopifnot(is.list(x))
  methods <- sapply(x, function(a) a$method)
  methodCheck(methods)

  #Name models
  if(is.null(names(x))){
    names(x) <- methods
  }
  i <- names(x)==""
  if(any(i)){
    names(x)[i] <- methods[i]
  }
  names(x) <- make.names(names(x), unique=TRUE)

  #Check params
  stopifnot(all(sapply(x, is.list)))
  return(x)
}

#' @title Check that the methods supplied by the user are valid caret methods
#' @description This function uses modelLookup from caret to ensure the list of methods supplied by the user are all models caret can fit.
#' @param x a list of user-supplied tuning parameters and methods
#' @importFrom caret modelLookup
#' @return NULL
methodCheck <- function(x){
  all_models <- unique(modelLookup()$model)
  bad_models <- setdiff(x, all_models)
  if(length(bad_models)>0){
    msg <- paste(bad_models, collapse=", ")
    stop(paste("The following models are not valid caret models:", msg))
  }
  return(invisible(NULL))
}

#' @title Check that the trainControl object supplied by the user is valid and has defined re-sampling indexes.
#' @description This function checks the user-supplied trainControl object and makes sure it has all the required fields.  If the resampling indexes are missing, it adds them to the model.  If savePredictions=FALSE or "none", this function sets it to "final".
#' @param x a trainControl object.
#' @param y the target for the model.  Used to determine resampling indexes.
#' @importFrom caret createResample createFolds createMultiFolds createDataPartition
#' @return NULL
trControlCheck <- function(x, y){

  if(!length(x$savePredictions) == 1){
    stop("Please pass exactly 1 argument to savePredictions, e.g. savePredictions='final'")
  }

  if(x$savePredictions == TRUE){
    warning("x$savePredictions == TRUE is depreciated. Setting to 'final' instead.")
    x$savePredictions <- "final"
  }

  if(!(x$savePredictions %in% c("all", "final"))){
    warning("trControl$savePredictions not 'all' or 'final'.  Setting to 'final' so we can ensemble the models.")
    x$savePredictions <- "final"
  }

  if(is.null(x$index)){
    warning("indexes not defined in trControl.  Attempting to set them ourselves, so each model in the ensemble will have the same resampling indexes.")
    if(x$method=="none"){
      stop("Models that aren't resampled cannot be ensembled.  All good ensemble methods rely on out-of sample data.  If you really need to ensemble without re-sampling, try the median or mean of the model's predictions.")

    } else if(x$method=="boot" | x$method=="adaptive_boot"){
      x$index <- createResample(y, times = x$number, list = TRUE)
    } else if(x$method=="cv" | x$method=="adaptive_cv"){
      x$index  <- createFolds(y, k = x$number, list = TRUE, returnTrain = TRUE)
    } else if(x$method=="repeatedcv"){
      x$index <- createMultiFolds(y, k = x$number, times = x$repeats)
    } else if(x$method=="LGOCV" | x$method=="adaptive_LGOCV"){
      x$index <- createDataPartition(
        y,
        times = x$number,
        p = 0.5,
        list = TRUE,
        groups = min(5, length(y)))
    } else {
      stop(paste0("caretList does not currently know how to handle cross-validation method='", x$method, "'. Please specify trControl$index manually"))
    }
  }
  return(x)
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train model.  Since there are 2 methods to call caret::train, this function also has 2 methods.
#' @param ... a set of arguments, as in the caret::train function
extractCaretTarget <- function(...){
  UseMethod("extractCaretTarget")
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.default function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.default model.
#' @param x an object where samples are in rows and features are in columns. This could be a simple matrix, data frame or other type (e.g. sparse matrix). See Details below.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param ... ignored
#' @method extractCaretTarget default
extractCaretTarget.default <- function(x, y, ...){
  return(y)
}

#' @title Extracts the target variable from a set of arguments headed to the caret::train.formula function.
#' @description This function extracts the y variable from a set of arguments headed to a caret::train.formula model.
#' @param form A formula of the form y ~ x1 + x2 + ...
#' @param data Data frame from which variables specified in formula are preferentially to be taken.
#' @param ... ignored
#' @method extractCaretTarget formula
extractCaretTarget.formula <- function(form, data, ...){
  y <- model.response(model.frame(form, data))
  names(y) <- NULL
  return(y)
}

#' Create a list of several train models from the caret package
#"
#' Build a list of train objects suitable for ensembling using the \code{\link{caretEnsemble}}
#' function.
#"
#' @param ... arguments to pass to \code{\link{train}}.  These arguments will determine which train method gets dispatched.
#' @param trControl a \code{\link{trainControl}} object.  We are going to intercept this object check that it has the "index" slot defined, and define the indexes if they are not.
#' @param methodList optional, a character vector of caret models to ensemble.  One of methodList or tuneList must be specified.
#' @param tuneList optional, a NAMED list of caretModelSpec objects. This much more flexible than methodList and allows the specificaiton of model-specific parameters (e.g. passing trace=FALSE to nnet)
#' @param continue_on_fail, logical, should a valid caretList be returned that excludes models that fail, default is FALSE
#' @return A list of \code{\link{train}} objects. If the model fails to build,
#' it is dropped from the list.
#' @importFrom caret trainControl train
#' @export
#' @examples
#' \dontrun{
#' myControl <- trainControl(method="cv", number=5)
#' caretList(
#'   Sepal.Length ~ Sepal.Width,
#'   head(iris, 50),
#'   methodList=c("glm", "lm"),
#'   trControl=myControl
#'   )
#' caretList(
#'   Sepal.Length ~ Sepal.Width,
#'   head(iris, 50), methodList=c("lm"),
#'   tuneList=list(
#'     nnet=caretModelSpec(method="nnet", trace=FALSE, tuneLength=1)
#'  ),
#'   trControl=myControl
#'   )
#'   }
caretList <- function(
  ...,
  trControl = NULL,
  methodList = NULL,
  tuneList = NULL,
  continue_on_fail = FALSE) {

  #Checks
  if(is.null(trControl)){
    trControl <- trainControl()
  }
  if(is.null(tuneList) & is.null(methodList)){
    stop("Please either define a methodList or tuneList")
  }
  if(!is.null(methodList) & any(duplicated(methodList))){
    warning("Duplicate entries in methodList.  Using unqiue methodList values.")
    methodList <- unique(methodList)
  }

  #Make methodList into a tuneList and add onto tuneList
  if(!is.null(methodList)){
    methodCheck(methodList)
    tuneList_extra <- lapply(methodList, caretModelSpec)
    tuneList <- c(tuneList, tuneList_extra)
  }

  #Make sure tuneList is valid
  tuneList <- tuneCheck(tuneList)

  #Capture global arguments for train as a list
  global_args <- list(...)

  #Add indexes to trControl if they are missing
  if(is.null(trControl$index)){
    target <- extractCaretTarget(...)
    trControl <- trControlCheck(x=trControl, y=target)
  }

  #Squish trControl back onto the global arguments list
  global_args[["trControl"]] <- trControl

  #Loop through the tuneLists and fit caret models with those specs
  modelList <- lapply(tuneList, function(m){
    model_args <- c(global_args, m)
    if(continue_on_fail == TRUE){
      model <- tryCatch(do.call(train, model_args), error=function(e) NULL)
    } else{
      model <- do.call(train, model_args)
    }
    return(model)
  })
  names(modelList) <- names(tuneList)
  nulls <- sapply(modelList, is.null)
  modelList <- modelList[!nulls]

  if(length(modelList)==0){
    stop("caret:train failed for all models.  Please inspect your data.")
  }
  class(modelList) <- "caretList"

  return(modelList)
}

#' @title Check if an object is a caretList object
#' @description Check if an object is a caretList object
#' @param object an R object
#' @export
is.caretList <- function(object){
  is(object, "caretList")
}

#' @title Create a matrix of predictions for each of the models in a caretList
#' @description Make a matrix of predictions from a list of caret models
#"
#' @param object an object of class caretList
#' @param verbose Logical. If FALSE no progress bar is printed if TRUE a progress
#' bar is shown. Default FALSE.
#' @param newdata New data for predictions.  It can be NULL, but this is ill-advised.
#' @param ... additional arguments to pass to predict.train. Pass the \code{newdata}
#' argument here, DO NOT PASS the "type" argument.  Classification models will
#' return probabilities if possible, and regression models will return "raw".
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pboptions
#' @export
#' @method predict caretList
predict.caretList <- function(object, newdata = NULL, ..., verbose = FALSE){

  if(is.null(newdata)){
    warning("Predicting without new data is not well supported.  Attempting to predict on the training data.")
    newdata <- object[[1]]$trainingData
    if(is.null(newdata)){
      stop("Could not find training data in the first model in the ensemble.")
    }
  }

  if(verbose == TRUE){
    pboptions(type = "txt", char = "*")
  } else if(verbose == FALSE){
    pboptions(type = "none")
  }
  preds <- pbsapply(object, function(x){
    type <- x$modelType
    if (type=="Classification"){
      if(x$control$classProbs){
        caret::predict.train(x, type="prob", newdata=newdata, ...)[, 2]
      } else{
        caret::predict.train(x, type="raw", newdata=newdata, ...)
      }
    } else if(type=="Regression"){
      caret::predict.train(x, type="raw", newdata=newdata, ...)
    } else{
      stop(paste("Unknown model type:", type))
    }
  })
  if(class(preds) != "matrix" & class(preds) != "data.frame"){
    if(class(preds) == "character" | class(preds) == "factor"){
      preds <- as.character(preds) # drop factorization
    }
    preds <- as.matrix(t(preds))
  }
  colnames(preds) <- make.names(sapply(object, function(x) x$method), unique=TRUE)
  return(preds)
}
