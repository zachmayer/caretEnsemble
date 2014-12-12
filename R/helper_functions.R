
#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model preditions
#'
#' @param x a vector of numerics
#' @param weights a vector of weights equal to length of x
#' @param normwt  a logical indicating whether the weights should be normalized to 1
#' @param na.rm a logical indicating how to handle missing values, default = FALSE
wtd.sd <- function (x, weights = NULL, normwt = FALSE, na.rm = FALSE) {
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sd(x))
  }
  if(length(weights) != length(x)){
    warning("length of the weights vector != the length of the x vector,
            weights are being recycled.")
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt){
    weights <- weights * length(x)/sum(weights)
  }
  xbar <- sum(weights * x)/sum(weights)
  out <- sqrt(sum(weights * ((x - xbar)^2))/(sum(weights)))
  return(out)
}

#' @title Check train models and extract their types
#' @description Check that a list of models are all train objects and are ready to be ensembled together
#'
#' @param list_of_models an object of class caretList
checkModels_extractTypes <- function(list_of_models){
  #TODO: Add helpful error messages

  #Check that we have a list of train models
  stopifnot(is(list_of_models, 'caretList'))
  stopifnot(all(sapply(list_of_models, is, 'train')))

  #Check that models have the same type
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]
  stopifnot(all(types==type)) #TODO: Maybe in the future we can combine reg and class models

  #Check that the model type is VALID
  stopifnot(all(types %in% c('Classification', 'Regression')))

  #Warn that we haven't yet implemented multiclass models
  # add a check that if this is null you didn't set savePredictions in the trainControl
  #TODO: add support for non-prob models (e.g. rFerns)
  if (type=='Classification' & length(unique(list_of_models[[1]]$pred$obs))!=2){
    if(is.null(unique(list_of_models[[1]]$pred$obs))){
      stop('No predictions saved by train. Please re-run models with trainControl set with savePredictions = TRUE.')
    } else {
      stop('Not yet implemented for multiclass problems')
    }
  }

  #Check that classification models saved probabilities
  #TODO: ALLOW NON PROB MODELS!
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
#' @param  list_of_models an object of class caretList
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
      indxLogic <- out[,name]==tune[,name]
      indxLogic[is.na(indxLogic)] <- FALSE
      out <- out[indxLogic,]
    }
    out <- out[order(out$Resample, out$rowIndex),]
    newModels[[i]] <- out
  }
  rm(modelLibrary)
  return(newModels)
}

#' @title Check predictions
#' @description Check that a list of predictions from caret models are all valid
#'
#' @param list_of_models a list of caret models to check
checkPreds <- function(list_of_models){
  stop('NOT IMPLEMENTED')
}

#' @title Make a prediction matrix from a list of models
#' @description Extract obs from one models, and a matrix of predictions from all other models, a
#' helper function
#'
#' @param  list_of_models an object of class caretList
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
