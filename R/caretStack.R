#' Combine several predictive models via stacking
#' 
#' Find a good linear combination of several classification or regression models, 
#' using either linear regression, elastic net regression, or greedy optimization.
#' 
#' Every model in the "library" must be a separate \code{train} object.  For 
#' example, if you wish to combine a random forests with several different 
#' values of mtry, you must build a model for each value of mtry.  If you
#' use several values of mtry in one train model, (e.g. tuneGrid =
#' expand.grid(.mtry=2:5)), caret will select the best value of mtry 
#' before we get a chance to include it in the ensemble.
#' 
#' @param all.models a list of caret models to ensemble.
#' @param optFUN the optimization function to use
#' @param ... additional arguments to pass to the optimization function
#' @export
#' @return S3 caretStack object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
caretStack <- function(all.models, ...){

  #TODO: Add progressbar argument and move optFUN to an all.models control argument
  
  #Libraries
  require('caret')
  require('pbapply')
  
  #Check the models, and make a matrix of obs and preds
  predobs <- makePredObsMatrix(all.models)
  
  #Build a caret model
  model <- train(predobs$preds, predobs$obs, ...)
  
  #Return final model
  out <- list(models=all.models, ens_model=model, error=model$results)
  class(out) <- 'caretStack'
  return(out) 
}

#' Make predictions from a caretStack. This function passes the data to each function in 
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param ensemble a caretStack to make predictions from.
#' @param ... arguments (including newdata) to pass to predict.train.
#' @export
predict.caretStack <- function(ensemble, newdata=NULL, ...){
  type <- checkModels_extractTypes(ensemble$models)
  preds <- multiPredict(ensemble$models, newdata=NULL, type)
  out <- predict(ensemble$ens_model, newdata=preds, ...)
  return(out)
}

