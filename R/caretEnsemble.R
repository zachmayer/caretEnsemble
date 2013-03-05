#' Combine several predictive models via weights
#' 
#' Find a good linear combination of several classification or regression models, 
#' using either linear regression, elastic net regression, or greedy optimization.
#' 
#' Every model in the "library" must be a separate \code{train} object.  For 
#' example, if you wish to combine a random forests with several different 
#' values of mtry, you must build a model for each value of mtry.  If you
#' use several values of mtry in one train model, (e.g. tuneGrid =
#' expand.grid(.mtry=2:5)), caret will select the best value of mtry 
#' before we get a chance to include it in the ensemble.  By default, 
#' RMSE is used to ensemble regression models, and AUC is used to ensemble
#' Classification models.  This function does not currently support multi-class
#' problems
#' 
#' @param all.models a list of caret models to ensemble.
#' @param optFUN the optimization function to use
#' @param ... additional arguments to pass to the optimization function
#' @export
#' @return S3 caretEnsemble object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
caretEnsemble <- function(all.models, optFUN=NULL, ...){
  
  #TODO: Add progressbar argument
  
  #Libraries
  require('caret')
  require('pbapply')
  
  #Check that we have a list of train models
  stopifnot(class(all.models)=='list')
  stopifnot(all(sapply(all.models, function(x) class(x)[1])=='train'))
  
  #Check that models have proper types
  types <- sapply(all.models, function(x) x$modelType)
  stopifnot(all(types %in% c('Classification', 'Regression')))
  stopifnot(all(types==types[1])) #Maybe in the future we can combine reg and class models
  if (types[1]=='Classification' & length(unique(all.models[[1]]$pred$obs))!=2){
    stop('Not yet implemented for multiclass problems')
  }
  
  #Check that classification models saved probabilities TODO: ALLOW NON PROB MODELS!
  if (types[1]=='Classification'){
    probModels <- sapply(all.models, function(x) modelLookup(x$method)[1,'probModel'])
    stopifnot(all(probModels))
    classProbs <- sapply(all.models, function(x) x$control$classProbs)
    stopifnot(all(classProbs))
  }
  
  #Check that all models saved their predictions so we can ensemble them
  stopifnot(all(sapply(all.models, function(x) x$control$savePredictions)))

  #Check that every model used the same resampling indexes
  indexes <- lapply(all.models, function(x) x$control$index)
  stopifnot(length(unique(indexes))==1)

  #Extract resampled predictions from each model
  modelLibrary <- lapply(all.models, function(x) {x$pred})
  
  #Extract the best tuning parameters from each model
  tunes <- lapply(all.models, function(x) {x$bestTune})
  
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
  modelLibrary <- newModels
  rm(newModels)
  
  #Insert checks here: observeds are all equal, row indexes are equal, Resamples are equal
  
  #Extract Observeds
  obs <- modelLibrary[[1]]$obs
  if (types[[1]]=='Classification'){
    positive <- as.character(unique(modelLibrary[[1]]$obs)[2]) #IMPROVE THIS!
  }
  
  #Extract predicted
  if (types[1]=='Regression'){
    preds <- sapply(modelLibrary, function(x) as.numeric(x$pred))
  } else if (types[1]=='Classification'){
    preds <- sapply(modelLibrary, function(x) as.numeric(x[,positive]))
  }
  
  #If the optimization function is NULL, choose defauls
  if (is.null(optFUN)){
    if (types[1]=='Classification') {
      optFUN <- greedOptAUC
    } else {
      optFUN <- greedOptRMSE
    }
  }
  
  #Determine weights
  weights <- optFUN(preds, obs, ...)
  weights[! is.finite(weights)] <- 0
  
  #Normalize weights
  weights <- weights/sum(weights)
  
  #Name weights
  names(weights) <- sapply(all.models, function(x) x$method)
  
  #Remove 0-weighted models
  keep <- which(weights != 0)
  
  #Determine RMSE
  if (types[1] == "Regression"){
    error <- RMSE(preds %*% weights, obs)
  } else {
    metric <- 'AUC'
    error <- colAUC(preds %*% weights, obs)
    names(error) <- 'AUC'
  }

  #Return final model
  out <- list(models=all.models[keep], weights=weights[keep], error=error)
  class(out) <- 'caretEnsemble'
  return(out) 
}

#' Make predictions from a caretEnsemble. This function passes the data to each function in 
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param ensemble A caretEnsemble to make predictions from.
#' @param ... arguments (including newdata) to pass to predict.train.
#' @export
predict.caretEnsemble <- function(ensemble, ...){
  #TODO: Add progressbar argument
  
  require('pbapply')
  types <- sapply(ensemble$models, function(x) x$modelType)
  stopifnot(all(types==types[1]))
  stopifnot(types[1] %in% c('Classification', 'Regression'))
  
  preds <- multiPredict(ensemble$models, types[1], ...)
  out <- as.numeric(preds %*% ensemble$weights)
  return(out)
}
