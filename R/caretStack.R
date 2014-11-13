#' @title Combine several predictive models via stacking
#'
#' @description Find a good linear combination of several classification or regression models,
#' using either linear regression, elastic net regression, or greedy optimization.
#'
#' @details Check the models, and make a matrix of obs and preds
#'
#' @param all.models a list of caret models to ensemble.
#' @param ... additional arguments to pass to the optimization function
#' @return S3 caretStack object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
#' @export
caretStack <- function(all.models, ...){

  predobs <- makePredObsMatrix(all.models)

  #Build a caret model
  model <- train(predobs$preds, predobs$obs, ...)

  #Return final model
  out <- list(models=all.models, ens_model=model, error=model$results)
  class(out) <- 'caretStack'
  return(out)
}

#' @title Make predictions from a caretStack
#' @description Make predictions from a caretStack. This function passes the data to each function in
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param object a  \code{\link{caretStack}} to make predictions from.
#' @param newdata a new dataframe to make predictions on
#' @param ... arguments to pass to \code{\link{predict.train}}.
#' @export
predict.caretStack <- function(object, newdata=NULL, ...){
  #TODO: grab type argument
  #TODO: rename my "type" variable
  type <- checkModels_extractTypes(object$models)
  preds <- multiPredict(object$models, newdata=newdata, type)
  out <- predict(object$ens_model, newdata=preds, ...)
  return(out)
}

