#' Make a matrix of predictions from a list of caret models
#' 
#' @param list_of_models a list of caret models to make predictions for
#' @param type Classification or Regression
#' @param ... additional arguments to pass to predict.train.  DO NOT PASS
#' the "type" argument.
#' @export
multiPredict <- function(list_of_models, type, ...){
  require('caret')
  require('pbapply')
  
  preds <- pbsapply(list_of_models, function(x){
    if (type=='Classification' & x$control$classProbs){
      predict(x, type='prob', ...)[,2]
    } else {
      predict(x, type='raw', ...)
    }
  })
  colnames(preds) <- make.names(sapply(list_of_models, function(x) x$method))
  
  return(preds)
}

#' Extract predictions for the best tune from a list of caret models
#' 
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