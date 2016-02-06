#####################################################
# Misc. Functions
#####################################################
#' @title Calculate a weighted standard deviation
#' @description Used to weight deviations among ensembled model preditions
#'
#' @param x a vector of numerics
#' @param w a vector of weights equal to length of x
#' @param na.rm a logical indicating how to handle missing values, default = FALSE
wtd.sd <- function (x, w = NULL, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]; x <- x[i]
    }
    n <- length(w)
    xWbar <- weighted.mean(x, w, na.rm = na.rm)
    wbar <- mean(w)
    out <- n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
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
#' @description Validate a caretList
#' @param list_of_models a list of caret models to check
#' @importFrom caret modelLookup
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
    probModels <- sapply(list_of_models, function(x) modelLookup(x$method)[1, "probModel"])
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

#' @title Extract the best predictions from a train object
#' @description Extract predictions for the best tune from a model
#' @param x a train object
#' @importFrom data.table data.table setorderv
bestPreds <- function(x){
  stopifnot(is(x, "train"))
  stopifnot({
    x$control$savePredictions %in% c("all", "final") |
      x$control$savePredictions == TRUE
  })
  a <- data.table(x$bestTune, key=names(x$bestTune))
  b <- data.table(x$pred, key=names(x$bestTune))
  b <- b[a, ]
  sink <- gc(reset=TRUE)
  setorderv(b, c("Resample", "rowIndex"))
  return(b)
}

#' @title Extract the best predictions from a list of train objects
#' @description Extract predictions for the best tune from a list of caret models
#' @param list_of_models an object of class caretList or a list of caret models
#' @importFrom pbapply pblapply
extractBestPreds <- function(list_of_models){
  out <- lapply(list_of_models, bestPreds)
  if(is.null(names(out))){
    names(out) <- make.names(sapply(list_of_models, function(x) x$method), unique=TRUE)
  }
  sink <- gc(reset=TRUE)
  return(out)
}

#' @title Make a prediction matrix from a list of models
#' @description Extract obs from one models, and a matrix of predictions from all other models, a
#' helper function
#'
#' @param  list_of_models an object of class caretList
#' @importFrom data.table set rbindlist dcast.data.table
makePredObsMatrix <- function(list_of_models){

  #caretList Checks
  check_caretList_classes(list_of_models)
  check_caretList_model_types(list_of_models)

  #Make a list of models
  modelLibrary <- extractBestPreds(list_of_models)
  model_names <- names(modelLibrary)

  #Model library checks
  check_bestpreds_resamples(modelLibrary) #Re-write with data.table?
  check_bestpreds_indexes(modelLibrary) #Re-write with data.table?
  check_bestpreds_obs(modelLibrary) #Re-write with data.table?
  check_bestpreds_preds(modelLibrary) #Re-write with data.table?

  #Extract model type (class or reg)
  type <- extractModelTypes(list_of_models)

  #Add names column
  for(i in seq_along(modelLibrary)){
    set(modelLibrary[[i]], j="modelname", value=names(modelLibrary)[[i]])
  }

  #Remove parameter columns
  keep <- Reduce(intersect, lapply(modelLibrary, names))
  for(i in seq_along(modelLibrary)){
    rem <- setdiff(names(modelLibrary[[i]]), keep)
    if(length(rem) > 0){
      for(r in rem){
        set(modelLibrary[[i]], j=r, value=NULL)
      }
    }
  }
  modelLibrary <- rbindlist(modelLibrary, fill=TRUE)

  #For classification models that produce probs, use the probs as preds
  #Otherwise, just use class predictions
  if (type=="Classification"){
    positive <- as.character(unique(modelLibrary$obs)[2]) #IMPROVE THIS!
    pos <- as.numeric(modelLibrary[[positive]])
    good_pos_values <- which(is.finite(pos))
    set(modelLibrary, j="pred", value=as.numeric(modelLibrary[["pred"]]))
    set(modelLibrary, i=good_pos_values, j="pred", value=modelLibrary[good_pos_values, positive, with=FALSE])
  }

  #Reshape wide for meta-modeling
  modelLibrary <- data.table::dcast.data.table(
    modelLibrary,
    obs + rowIndex + Resample ~ modelname,
    value.var = "pred"
  )

  #Return
  return(list(obs=modelLibrary$obs, preds=as.matrix(modelLibrary[, model_names, with=FALSE]), type=type))
}
