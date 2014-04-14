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
  
  #TODO: Add progressbar argument and move optFUN to an all.models control argument
  
  #Libraries
  require('caret')
  require('pbapply')

  #Check the models, and make a matrix of obs and preds
  predobs <- makePredObsMatrix(all.models)

  #If the optimization function is NULL, choose default
  if (is.null(optFUN)){
    if (predobs$type=='Classification') {
      optFUN <- greedOptAUC
    } else { optFUN <- greedOptRMSE }
  }
  
  #Determine weights
  weights <- optFUN(predobs$preds, predobs$obs, ...)
  weights[! is.finite(weights)] <- 0
  
  #Normalize and name weights
  weights <- weights/sum(weights)
  names(weights) <- sapply(all.models, function(x) x$method)
  
  #Remove 0-weighted models
  keep <- which(weights != 0)
  
  #Determine RMSE
  if (predobs$type == "Regression"){
    error <- RMSE(predobs$preds %*% weights, predobs$obs)
    names(error) <- 'RMSE'
  } else {
    metric <- 'AUC'
    error <- colAUC(predobs$preds %*% weights, predobs$obs)
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
#' @param ensemble a caretEnsemble to make predictions from.
#' @param keepNA a logical indicating whether predictions should be made for all 
#' cases where sufficient data exists or only for complete cases across all models
#' @param ... arguments (including newdata) to pass to predict.train. These arguments 
#' must be named
#' @export
#' @method
predict.caretEnsemble <- function(ensemble, keepNA = TRUE, ...){
  type <- checkModels_extractTypes(ensemble$models)
  preds <- multiPredict(ensemble$models, type, ...)
  if(keepNA == TRUE){
    message("Predictions being made only for cases with complete data")
    out <- as.numeric(preds %*% ensemble$weights)
  } else {
    message("Predictions being made only from models with available data")
    conf <- ifelse(is.na(preds), NA, 1)
    conf <- sweep(conf, MARGIN=2, ensemble$weights,`*`)
    conf <- apply(conf, 1, function(x) x / sum(x, na.rm=TRUE))
    conf <- t(conf); conf[is.na(conf)] <- 0
    preds <- apply(preds, 1, function(x){weighted.mean(x, w=ensemble$weights, na.rm=TRUE)})
    out <- list(predicted = preds, weight = conf)
  }
  return(out)
}

#' Summarize the results of caretEnsemble for the user.
#' @param ensemble a caretEnsemble to make predictions from.
#' @export
summary.caretEnsemble <- function(ensemble){
  types <- names(ensemble$models)
  types <- paste(types, collapse = ", ")
  wghts <- ensemble$weights
  metric <- names(ensemble$error)
  val <- ensemble$error[[1]]
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(wghts, collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4), "\n"))
  
  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModRes(ensemble))
}

#' Extract the model accuracy metrics of the individual models in an ensemble object.
#' @param ensemble a caretEnsemble to make predictions from.
#' @export
extractModRes <- function(ensemble){
  if(class(ensemble) != "caretEnsemble") stop("extractModRes requires a caretEnsemble object")
  modRes <- data.frame(method = names(ensemble$models), 
                       metric = NA, 
                       metricSD = NA)
  
  for(i in names(ensemble$models)){
    dat <- ensemble$models[[i]]$results
    metric <- ensemble$models[[i]]$metric
    SDVAR <- paste0(ensemble$models[[i]]$metric, "SD")
    best <- max(dat[, metric])
    bestSD <- max(dat[dat[, metric] == best, SDVAR])
    modRes[modRes[, "method"] == i, "metric"] <- best
    modRes[modRes[, "method"] == i, "metricSD"] <- bestSD
  }
  return(modRes)
}

