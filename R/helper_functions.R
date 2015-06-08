#####################################################
# Misc. Functions
#####################################################
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

#####################################################
# caretList check functions
#####################################################
#' @title Checks caretList model classes
#' @description This function checks caretList classes
#'
#' @param list_of_models a list of caret models to check
check_caretList_classes <- function(list_of_models){

  #Check that we have a list of train models
  stopifnot(is(list_of_models, "caretList"))
  stopifnot(all(sapply(list_of_models, is, "train")))
  return(invisible(NULL))
}

#' @title Checks that caretList models are all of the same type.
#'
#' @param list_of_models a list of caret models to check
check_caretList_model_types <- function(list_of_models){
  #Check that models have the same type
  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]
  stopifnot(all(types==type)) #TODO: Maybe in the future we can combine reg and class models

  #Check that the model type is VALID
  stopifnot(all(types %in% c("Classification", "Regression")))

  #Warn that we haven"t yet implemented multiclass models
  # add a check that if this is null you didn"t set savePredictions in the trainControl
  #TODO: add support for non-prob models (e.g. rFerns)
  if (type=="Classification" & length(unique(list_of_models[[1]]$pred$obs))!=2){
    if(is.null(unique(list_of_models[[1]]$pred$obs))){
      stop("No predictions saved by train. Please re-run models with trainControl set with savePredictions = TRUE.")
    } else {
      stop("Not yet implemented for multiclass problems")
    }
  }

  #Check that classification models saved probabilities
  #TODO: ALLOW NON PROB MODELS!
  if (type=="Classification"){
    probModels <- sapply(list_of_models, function(x) modelLookup(x$method)[1,"probModel"])
    if(!all(probModels)) stop("All models for classification must be able to generate class probabilities.")
    classProbs <- sapply(list_of_models, function(x) x$control$classProbs)
    if(!all(classProbs)){
      bad_models <- names(list_of_models)[!classProbs]
      bad_models <- paste(bad_models, collapse=", ")
      stop(
        paste0(
          "The following models were fit by caret::train with no class probabilities: ",
          bad_models,
          ".\nPlease re-fit them with trainControl(classProbs=TRUE)"))
    }
  }
  return(invisible(NULL))
}

#' @title Check resamples
#' @description Check that the resamples from a caretList are valid
#'
#' @param modelLibrary a list of predictins from caret models
check_bestpreds_resamples <- function(modelLibrary){
  #TODO: ID which model(s) have bad row indexes
  resamples <- lapply(modelLibrary, function(x) x[["Resample"]])
  names(resamples) <- names(modelLibrary)
  check <- length(unique(resamples))
  if(check != 1){
    stop("Component models do not have the same re-sampling strategies")
  }
  return(invisible(NULL))
}

#' @title Check row indexes
#' @description Check that the row indexes from a caretList are valid
#'
#' @param modelLibrary a list of predictins from caret models
check_bestpreds_indexes <- function(modelLibrary){
  #TODO: ID which model(s) have bad row indexes
  rows <- lapply(modelLibrary, function(x) x[["rowIndex"]])
  names(rows) <- names(modelLibrary)
  check <- length(unique(rows))
  if(check != 1){
    stop("Re-sampled predictions from each component model do not use the same rowIndexs from the origial dataset")
  }
  return(invisible(NULL))
}

#' @title Check observeds
#' @description Check that a list of observed values from a caretList are valid
#'
#' @param modelLibrary a list of predictins from caret models
check_bestpreds_obs <- function(modelLibrary){
  #TODO: ID which model(s) have bad row indexes
  obs <- lapply(modelLibrary, function(x) x[["obs"]])
  names(obs) <- names(modelLibrary)
  check <- length(unique(obs))
  if(check != 1){
    stop("Observed values for each component model are not the same.  Please re-train the models with the same Y variable")
  }
  return(invisible(NULL))
}

#' @title Check predictions
#' @description Check that a list of predictions from a caretList are valid
#'
#' @param modelLibrary a list of predictins from caret models
check_bestpreds_preds <- function(modelLibrary){
  #TODO: ID which model(s) have bad preds
  #TODO: Regression models should be numeric, classification models should have numeric class probs
  pred <- lapply(modelLibrary, function(x) x[["pred"]])
  names(pred) <- names(modelLibrary)
  classes <- unique(sapply(pred, class))
  check <- length(classes)
  if(check != 1){
    stop(
      paste0(
        "Component models do not all have the same type of predicitons.  Predictions are a mix of ",
        paste(classes, collapse=", "),
        ".")
    )
  }
  return(invisible(NULL))
}

#####################################################
# Extraction functions
#####################################################
#' @title Extracts the model types from a list of train model
#' @description Extracts the model types from a list of train model
#'
#' @param list_of_models an object of class caretList
extractModelTypes <- function(list_of_models){

  types <- sapply(list_of_models, function(x) x$modelType)
  type <- types[1]

  #TODO: Maybe in the future we can combine reg and class models
  #Also, this check is redundant, but I think that"s ok
  stopifnot(all(types==type))
  stopifnot(all(types %in% c("Classification", "Regression")))
  return(type)
}

#' @title Extract the best predictions from a list of train objects
#' @description Extract predictions for the best tune from a list of caret models
#' @param  list_of_models an object of class caretList
extractBestPreds <- function(list_of_models){

  #TODO: use data.table for faster sorting?
  #TODO: add an optional progress bar?
  #Extract resampled predictions from each model
  modelLibrary <- lapply(list_of_models, function(x) x$pred)

  #Extract the best tuning parameters from each model
  tunes <- lapply(list_of_models, function(x) x$bestTune)

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
  names(newModels) <- names(list_of_models)
  return(newModels)
}

#' @title Make a prediction matrix from a list of models
#' @description Extract obs from one models, and a matrix of predictions from all other models, a
#' helper function
#'
#' @param  list_of_models an object of class caretList
makePredObsMatrix <- function(list_of_models){

  #caretList Checks
  check_caretList_classes(list_of_models)
  check_caretList_model_types(list_of_models)

  #Make a list of models
  modelLibrary <- extractBestPreds(list_of_models)

  #Model library checks
  check_bestpreds_resamples(modelLibrary)
  check_bestpreds_indexes(modelLibrary)
  check_bestpreds_obs(modelLibrary)
  check_bestpreds_preds(modelLibrary)

  #Extract model type (class or reg)
  type <- extractModelTypes(list_of_models)

  #Extract observations from the frist model in the list
  obs <- modelLibrary[[1]]$obs
  if (type=="Classification"){
    positive <- as.character(unique(modelLibrary[[1]]$obs)[2]) #IMPROVE THIS!
  }

  #Extract predicteds
  if (type=="Regression"){
    preds <- sapply(modelLibrary, function(x) as.numeric(x$pred))
  } else if (type=="Classification"){
    preds <- sapply(modelLibrary, function(x) as.numeric(x[,positive]))
  }

  #Name the predicteds and return
  colnames(preds) <- make.names(sapply(list_of_models, function(x) x$method), unique=TRUE)
  return(list(obs=obs, preds=preds, type=type))
}
