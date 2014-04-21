#TODO: Move ensembling functions to their own document
#TODO: Move predictions functions to their own document

#' @title Check train models and extract their types
#' @description Check that a list of models are all train objects and are ready to be ensembled together
#' 
#' @param list_of_models a list of caret models to check
#' @export
checkModels_extractTypes <- function(list_of_models){
  #require('caret')
  #TODO: Add helpful error messages
  #Check that we have a list of train models
  stopifnot(class(list_of_models)=='list')
  stopifnot(all(sapply(list_of_models, function(x) class(x)[1])=='train'))
  
  #Check that models have the same type
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]
  stopifnot(all(types==type)) #Maybe in the future we can combine reg and class models
  
  #Check that the model type is VALID
  stopifnot(all(types %in% c('Classification', 'Regression')))

  #Warn that we haven't yet implemented multiclass models
  if (type=='Classification' & length(unique(list_of_models[[1]]$pred$obs))!=2){
    stop('Not yet implemented for multiclass problems')
  }
  
  #Check that classification models saved probabilities TODO: ALLOW NON PROB MODELS!
  if (type=='Classification'){
    probModels <- sapply(list_of_models, function(x) modelLookup(x$method)[1,'probModel'])
    stopifnot(all(probModels))
    classProbs <- sapply(list_of_models, function(x) x$control$classProbs)
    stopifnot(all(classProbs))
  }
  
  #Check that all models saved their predictions so we can ensemble them
  stopifnot(all(sapply(list_of_models, function(x) x$control$savePredictions)))
  
  #Check that every model used the same resampling indexes
  indexes <- lapply(list_of_models, function(x) x$control$index)
  stopifnot(length(unique(indexes))==1)
  
  return(type)
}

#' @title Extract the best predictions from a list of train objects
#' @description Extract predictions for the best tune from a list of caret models
#' @param list_of_models a list of caret models to extract predictions from
#' @export
extractBestPreds <- function(list_of_models){
  
  #TODO: add an optional progress bar?
  
  #Extract resampled predictions from each model
  modelLibrary <- lapply(list_of_models, function(x) {x$pred})
  
  #Extract the best tuning parameters from each model
  tunes <- lapply(list_of_models, function(x) {x$bestTune})
  
  #Subset the resampled predictions to the model with the best tune and sort
  newModels <- lapply(1:length(modelLibrary), function(x) NA)
  for (i in 1:length(modelLibrary)){
    out <- modelLibrary[[i]]
    tune <- tunes[[i]]
    for (name in names(tune)){
      out <- out[out[,name]==tune[,name],]
    }
    out <- out[order(out$Resample, out$rowIndex),]
    newModels[[i]] <- out
  }
  rm(modelLibrary)
  return(newModels)
}

#' @description Check that a list of predictions from caret models are all valid
#' 
#' @param list_of_models a list of caret models to check
#' @export
#' 
checkPreds <- function(list_of_models){
  stop('NOT IMPLEMENTED')
}

#' @description Extract obs from one models, and a matrix of predictions from all other models
#' 
#' @param list_of_models a list of caret models to extract predictions from
#' @export
makePredObsMatrix <- function(list_of_models){
  
  #Check models and extract type (class or reg)
  type <- checkModels_extractTypes(list_of_models)
  
  #Make a list of models
  modelLibrary <- extractBestPreds(list_of_models)
  
  #Insert checks here: observeds are all equal, row indexes are equal, Resamples are equal
  
  #Extract observations from the frist model in the list
  obs <- modelLibrary[[1]]$obs
  if (type=='Classification'){
    positive <- as.character(unique(modelLibrary[[1]]$obs)[2]) #IMPROVE THIS!
  }
  
  #Extract predicteds
  if (type=='Regression'){
    preds <- sapply(modelLibrary, function(x) as.numeric(x$pred))
  } else if (type=='Classification'){
    preds <- sapply(modelLibrary, function(x) as.numeric(x[,positive]))
  }
  
  #Name the predicteds and return
  colnames(preds) <- make.names(sapply(list_of_models, function(x) x$method), unique=TRUE)
  return(list(obs=obs, preds=preds, type=type)) 
}

#' @description Make a matrix of predictions from a list of caret models
#' 
#' @param list_of_models a list of caret models to make predictions for
#' @param type Classification or Regression
#' @param ... additional arguments to pass to predict.train.  DO NOT PASS
#' the "type" argument.  Classsification models will returns probabilities
#' if possible, and regression models will return "raw".
#' @export
multiPredict <- function(list_of_models, type, newdata=NULL, ...){
  
  #TODO: Add progressbar argument
  
  require('caret')
  require('pbapply')
  
  preds <- pbsapply(list_of_models, function(x){
    if (type=='Classification' & x$control$classProbs){
      predict(x, type='prob', newdata=newdata, ...)[,2]
    } else {
      predict(x, type='raw', newdata=newdata, ...)
    }
  })
  colnames(preds) <- make.names(sapply(list_of_models, function(x) x$method), unique=TRUE)
  
  return(preds)
}
