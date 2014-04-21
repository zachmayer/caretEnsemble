setOldClass("train")

##' @title Class "caretEnsemble" of ensembled train objects from the caret package
##'
##' @description Ensembled model from input train objects. 
##' 
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{caretEnsemble}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{models - a list of the original models to be ensembled}
##' \item{weights - a matrix with the weights for each model}
##' \item{error - the final accuracy metric of the ensembled models}
##' }
##' @seealso \code{\link{caretEnsemble}}
##' @keywords classes
##' @examples
##'
##' showClass("caretEnsemble")
##' methods(class="caretEnsemble")
##' @exportClass
caretEnsemble <- setClass("caretEnsemble", representation(models = "list", 
                                                  weights = "data.frame", 
                                          error = "numeric"),
                  S3methods=TRUE)

#' @title Combine several predictive models via weights
#' 
#' @description Find a good linear combination of several classification or regression models, 
#' using either linear regression, elastic net regression, or greedy optimization.
#' 
#' @details Every model in the "library" must be a separate \code{train} object.  For 
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
#' @return a \code{\linkS4class{caretEnsemble}} object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
#' @export
caretEnsemble <- function(all.models, optFUN=NULL, ...){
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
#' @param object a \code{\linkS4class{caretEnsemble}} to make predictions from.
#' @param keepNA a logical indicating whether predictions should be made for all 
#' cases where sufficient data exists or only for complete cases across all models
#' @param newdata a dataframe of new data to make predictions on from the \code{\linkS4class{caretEnsemble}}
#' @param ... arguments (including newdata) to pass to predict.train. These arguments 
#' must be named
#' @export
predict.caretEnsemble <- function(object, keepNA = TRUE, newdata = NULL, ...){
  type <- checkModels_extractTypes(object$models)
  preds <- multiPredict(object$models, type, newdata = newdata, ...)
  if(keepNA == TRUE){
    message("Predictions being made only for cases with complete data")
    out <- as.numeric(preds %*% object$weights)
  } else {
    message("Predictions being made only from models with available data")
    conf <- ifelse(is.na(preds), NA, 1)
    conf <- sweep(conf, MARGIN=2, object$weights,`*`)
    conf <- apply(conf, 1, function(x) x / sum(x, na.rm=TRUE))
    conf <- t(conf); conf[is.na(conf)] <- 0
    preds <- apply(preds, 1, function(x){weighted.mean(x, w=object$weights, na.rm=TRUE)})
    out <- list(predicted = preds, weight = conf)
  }
  return(out)
}


#' @title Summarize the results of caretEnsemble for the user.
#' @param object a \code{\linkS4class{caretEnsemble}} to make predictions from.
#' @param ... optional additional parameters. 
#' @export
summary.caretEnsemble <- function(object, ...){
  types <- names(object$models)
  types <- paste(types, collapse = ", ")
  wghts <- object$weights
  metric <- names(object$error)
  val <- object$error[[1]]
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(wghts, collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4), "\n"))
  
  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModRes(object))
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

