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
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:50,1:2],
#'   y=iris[1:50,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' caretStack(models, method="glm")
#' }
caretStack <- function(all.models, ...){

  predobs <- makePredObsMatrix(all.models)

  #Build a caret model
  model <- train(predobs$preds, predobs$obs, ...)

  #Return final model
  out <- list(models=all.models, ens_model=model, error=model$results)
  class(out) <- "caretStack"
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
#' @method predict caretStack
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:100,1:2],
#'   y=iris[1:100,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method="lm")
#' RMSE(predict(meta_model, iris[101:150,1:2]), iris[101:150,3])
#' }
predict.caretStack <- function(object, newdata=NULL, ...){
  stopifnot(is(object$models, "caretList"))
  type <- extractModelTypes(object$models)
  preds <- predict(object$models, newdata=newdata)
  out <- predict(object$ens_model, newdata=preds, ...)
  return(out)
}

#' @title Summarize a caretStack object
#' @description This is a function to summarize a caretStack.
#' @param object An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:100,1:2],
#'   y=iris[1:100,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method="lm")
#' summary(meta_model)
#' }
summary.caretStack <- function(object, ...){
  summary(object$ens_model)
}

#' @title Print a caretStack object
#' @description This is a function to print a caretStack.
#' @param x An object of class caretStack
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:100,1:2],
#'   y=iris[1:100,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method="lm")
#' print(meta_model)
#' }
print.caretStack <- function(x, ...){
  n <- length(x$models)
  cat(paste(
    "A",
    x$ens_model$method,
    "ensemble of 2 base models:",
    paste(sapply(x$models, function(x) x$method), collapse=", ")))
  cat("\n\nEnsemble results:\n")
  print(x$ens_model)
}

#' @title Plot a caretStack object
#' @description This is a function to plot a caretStack.
#' @param x An object of class caretStack
#' @param ... passed to plot
#' @export
#' @method plot caretStack
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:100,1:2],
#'   y=iris[1:100,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method="rpart", tuneLength=2)
#' plot(meta_model)
#' }
plot.caretStack <- function(x, ...){
  plot(x$ens_model, ...)
}

#' @title Comparison dotplot for a caretStack object
#' @description This is a function to make a dotplot from a caretStack.  It uses dotplot from the caret package on all the models in the ensemble, plus the final ensemble model.  At the moment, this function only works if the ensembling model has the same number of resamples as the component models.
#' @param x An object of class caretStack
#' @param data passed to dotplot
#' @param ... passed to dotplot
#' @importFrom lattice dotplot
#' @examples
#' \dontrun{
#' set.seed(42)
#' library("rpart")
#' models <- caretList(
#'   x=iris[1:100,1:2],
#'   y=iris[1:100,3],
#'   trControl=trainControl(method="cv"),
#'   methodList=c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method="lm", trControl=trainControl(method="cv"))
#' dotplot.caretStack(meta_model)
#' }
dotplot.caretStack <- function(x, data=NULL, ...){
  dotplot(resamples(x$models), data=data, ...)
}
