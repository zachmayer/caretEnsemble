##' @title Class "caretStack" of ensembled train objects from the caret package
##'
##'
##' @name caretStack-class
##' @aliases caretStack-class
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{caretStack}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{models - a list of the original models to be ensembled}
##' \item{ens_model - a \code{\link{train}} object}
##' \item{error - the final accuracy metric of the ensembled models}
##' }
##' @seealso \code{\link{caretEnsemble}}
##' @keywords classes
##' @examples
##'
##' showClass("caretEnsemble")
##' methods(class="caretEnsemble")
##' @export
caretStack <- setClass("caretStack", representation(models = "list", 
                                                    ens_model = "train", 
                                                          error = "numeric"),
                          S3methods=TRUE)


#' @title Combine several predictive models via stacking
#' 
#' @description Find a good linear combination of several classification or regression models, 
#' using either linear regression, elastic net regression, or greedy optimization.
#' 
#' @details Every model in the "library" must be a separate \code{train} object.  For 
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
  
  #Check the models, and make a matrix of obs and preds
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
#' @param ensemble a  \code{\linkS4class{caretStack}} to make predictions from.
#' @param ... arguments (including newdata) to pass to predict.train.
#' @export
#' @method predict caretStack
predict.caretStack <- function(ensemble, newdata=NULL, ...){
  #TODO: grab type argument
  #TODO: rename my "type" variable
  type <- checkModels_extractTypes(ensemble$models)
  preds <- multiPredict(ensemble$models, newdata=newdata, type)
  out <- predict(ensemble$ens_model, newdata=preds, ...)
  return(out)
}

