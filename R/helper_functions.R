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


