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
  
  stop('Not implemented')
  #TODO: Add progressbar argument
  
  #Libraries
  require('caret')
  require('pbapply')
  

  #Return final model
  class(out) <- 'caretStack'
  return(out) 
}

#' Make predictions from a caretStack. This function passes the data to each function in 
#' turn to make a matrix of predictions...
#' @param stack A caretStack to make predictions from.
#' @param ... arguments (including newdata) to pass to predict.train. DO NOT SPECIFY the
#' "type" argument.
#' @export
predict.caretStack <- function(stack, ...){
  #TODO: Add progressbar argument
  
  stop('Not implemented')
  require('pbapply')
  types <- sapply(stack$models, function(x) x$modelType)
  stopifnot(all(types==types[1]))
  stopifnot(types[1] %in% c('Classification', 'Regression'))
  
  preds <- multiPredict(stack$models, types, ...)
  
  if (types[1]=='Classification' & stack$stack_model$control$classProbs){
    out <- predict(stack$stack_model, newdata=preds, type='prob', ...)
  } else {
    out <- predict(stack$stack_model, newdata=preds, type='raw', ...)
  }

  return(out)
}
