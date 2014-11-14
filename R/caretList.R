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
trControlCheck <- function(x, n){

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
      r <- createResample(y, times = x$number, list = TRUE)
      x$index <- r
    } else if(x$method=='cv'){
      r <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)
      x$index <- r
    } else if(x$method=='repeatedcv'){
      r <- createMultiFolds(y, k = 10, times = 5)
      x$index <- r
    } else if(x$method=='LGOCV'){
      r <- createDataPartition(
        y,
        times = 1,
        p = 0.5,
        list = TRUE,
        groups = min(5, length(y)))
      x$index <- r
    } else {
      stop(paste0('buildModels does not currently know how to handle cross-validation method=', x$method, '. Please specify trControl$index manually'))
    }

  }






  return(x)
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

  #Define the index slot of trControl if it is missing
  trControl <- trControlCheck(trControl)

  #Squish trControl back onto the global arguments list
  global_args <- list(...)
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
