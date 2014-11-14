#' Extract the tuning parameters from a list provided by the user
#'
#' @param x a list of tuning parameters and methods passed by the user
#' @return A simple list of the tuning parameters
tuneCheck <- function(x){
  warning('NOT IMPLEMENTED')
  return(invisible(NULL))
}
methodCheck <- function(x){
  warning('NOT IMPLEMENTED')
  return(invisible(NULL))
}
trControlCheck <- function(x, y){

  if(!x$savePredictions){
    warning('trControl$savePredictions=FALSE.  Setting to TRUE so we can ensemble the models.')
    x$savePredictions <- TRUE
  }

  if(!x$classProbs){
    warning('trControl$classProbs=FALSE.  Setting to TRUE so we can ensemble the models.')
    x$classProbs <- TRUE
  }

  if(is.null(x$index)){
    warning('indexes not defined in trControl.  Attempting to set them ourselves, so each model in the ensemble will have the same resampling indexes.')
    if(x$method=='none'){
      stop("Models that aren't resampled cannot be ensembled.  All good ensemble methods rely on out-of sample data.  If you really need to ensemble without re-sampling, try the median or mean of the model's predictions.")

    } else if(x$method=='boot'){
      x$index <- createResample(y, times = x$number, list = TRUE)
    } else if(x$method=='cv'){
      x$index  <- createFolds(y, k = x$number, list = TRUE, returnTrain = TRUE)
    } else if(x$method=='repeatedcv'){
      x$index <- createMultiFolds(y, k = x$number, times = x$repeats)
    } else if(x$method=='LGOCV'){
      x$index <- createDataPartition(
        y,
        times = x$number,
        p = 0.5,
        list = TRUE,
        groups = min(5, length(y)))
    } else {
      stop(paste0("buildModels does not currently know how to handle cross-validation method='", x$method, "'. Please specify trControl$index manually"))
    }
  }
  return(x)
}
extractCaretArgsY <- function(x){
  if(is.data.frame(x[[1]])){
    if(is.null(x$y)){
      stop('train passed x argument but no y.')
    }
    return(x$y)
  }
  if('formula' %in% class(x[[1]])){
    flma <- x[[1]]
    resp <- attr(terms(flma), 'response')
    if(resp==0){
      stop('train passed a 1-sided formula.  Please specify a response.')
    }
    return(attr(terms(flma), 'term.labels')[resp])
  }
  x_class <- paste0("'", paste(class(x[[1]]), collapse="', '"), "'")
  stop(paste("Don't know how to extract response from object of class", x_class))
}

#' Create a list of several train models from the caret package
#'
#' Build a list of train objects suitable for ensembling using the \code{\link{caretEnsemble}}
#' function.
#'
#' @param ... arguments to pass to \code{\link{train}}.  These arguments will determine which train method gets dispatched.
#' @param trControl a \code{\link{trainControl}} object.  We are going to intercept this object check that it has the "index" slot defined, and define the indexes if they are not.
#' @param methodList optional, a character vector of caret models to ensemble.  One of methodList or tuneList must be specified.
#' @param tuneList optional, a NAMED list of the length of \code{methodList} with model-specific arguments to pass to train.  The can be arguments for the train function (e.g. tuneLength=6) or arguments passed through train to the modeling funcion (e.g. verbose=FALSE for a \code{\link{gbm}} model).  The names in the tuneList must match the methods in methodList, but do not need to be in the same order.  One of methodList or tuneList must be specified.
#' @return A list of \code{\link{train}} objects
#' @import caret
#' @export
buildModels <- function(..., trControl = trainControl(), methodList = NULL, tuneList = NULL) {

  #Decide how we're going to fit models
  if(is.null(tuneList) & is.null(methodList)){
    stop('Please either define a methodList or tuneList')
  }
  if((!is.null(tuneList)) & (!is.null(methodList))){
    warning('Both tuneList and methodList defined.  Ignoring methodList')
  }

  #Make sure tuneList is valid
  if(!is.null(tuneList)){
    methodList <- names(tuneList)
    tuneCheck(tuneList)
  }

  #Check that the methods are valid
  methodCheck(methodList)

  #Capture global arguments for train as a list
  global_args <- list(...)

  #Define the index slot of trControl if it is missing
  if(is.null(trControl$index)){
    y <- extractCaretArgsY(global_args)
    trControl <- trControlCheck(x=trControl, y=y)
  }

  #Squish trControl back onto the global arguments list
  global_args[['trControl']] <- trControl

  #If the tuneList is missing, fit each model with 100% default caret params
  modelList <- lapply(methodList, function(i){

    #Start with global args
    model_args <- global_args

    #Add tuneList args, if needed
    if(!is.null(tuneList)){
      model_args <- c(model_args, tuneList[[i]])
    }

    #Finally, add method.
    model_args[['method']] <- i

    #We now have a complete args list for train, so we can dispatch with do.cal
    model <- do.call(train, model_args)
    return(model)
  })

  names(modelList) <- methodList
  return(modelList)
}
