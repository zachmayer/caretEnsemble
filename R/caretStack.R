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
#' @references Caruana, R., Niculescu-Mizil, A., Crew, G., & Ksikes, A. (2004). Ensemble Selection from Libraries of Models. \url{https://www.cs.cornell.edu/~caruana/ctp/ct.papers/caruana.icml04.icdm06long.pdf}
#' @export
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x = iris[1:50, 1:2],
#'   y = iris[1:50, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' caretStack(models, method = "glm")
#' }
caretStack <- function(all.models, ...) {
  predobs <- makePredObsMatrix(all.models)

  # Build a caret model
  model <- train(predobs$preds, predobs$obs, ...)

  # Return final model
  out <- list(models = all.models, ens_model = model, error = model$results)
  class(out) <- "caretStack"
  return(out)
}

#' @title Make predictions from a caretStack
#' @description Make predictions from a caretStack. This function passes the data to each function in
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param object a  \code{\link{caretStack}} to make predictions from.
#' @param newdata a new dataframe to make predictions on
#' @param se logical, should prediction errors be produced? Default is false.
#' @param level tolerance/confidence level
#' @param return_weights a logical indicating whether prediction weights for each model
#' should be returned
#' @param na.action the method for handling missing data passed to \code{\link[caret]{predict.train}}.
#' @param ... arguments to pass to \code{\link[caret]{predict.train}}.
#' @export
#' @details Prediction weights are defined as variable importance in the stacked
#' caret model. This is not available for all cases such as where the library
#' model predictions are transformed before being passed to the stacking model.
#' @method predict caretStack
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' RMSE(predict(meta_model, iris[101:150, 1:2]), iris[101:150, 3])
#' }
predict.caretStack <- function(
    object, newdata = NULL,
    se = FALSE, level = 0.95,
    return_weights = FALSE,
    na.action = na.omit,
    ...) {
  stopifnot(is(object$models, "caretList"))
  type <- extractModelTypes(object$models)

  preds <- predict(object$models, newdata = newdata, na.action = na.action)

  if (type == "Classification") {
    # We have to use the same columns (variables) as the ones used
    # to train the model.
    # If the user has specified a class to exclude within the range
    # of 1 to num_classes, exclude that class from the predictions.
    # Otherwise, include all classes.
    column_names <- colnames(preds)

    # TODO: Validate that all object$models[ have the same obs
    # TODO: Validate that all object$models[ have the same pred levels
    num_classes <- length(levels(object$models[[1]]$pred$obs))
    if (getMulticlassExcludedLevel() >= 1 && getMulticlassExcludedLevel() <= num_classes) {
      classes_included <- levels(object$models[[1]]$pred$obs)[-getMulticlassExcludedLevel()]
    } else {
      warning("Value for caret.ensemble.multiclass.excluded.level is outside the range between 1 and the number of classes. Returning all classes.")
      classes_included <- levels(object$models[[1]]$pred$obs)
    }
    pattern <- paste(classes_included, collapse = "|")
    # Remove columns that are associated with the class that was excluded
    filtered_column_names <- grep(pattern, column_names, value = TRUE)
    preds <- preds[, filtered_column_names, drop = FALSE]

    est <- predict(object$ens_model, newdata = preds, na.action = na.action, ...)
  } else {
    est <- predict(object$ens_model, newdata = preds, ...)
  }

  if (se || return_weights) {
    imp <- varImp(object$ens_model)$importance
    weights <- as.list(as.data.frame(imp))
    methods <- colnames(preds)
    weights <- lapply(weights, function(class_weights) {
      # ensure that we have a numeric vector
      class_weights <- ifelse(is.finite(class_weights), class_weights, 0)
      # normalize weights
      class_weights <- class_weights / sum(class_weights)
      names(class_weights) <- row.names(imp)
      # set 0 weights for methods that are not present in varImp
      for (m in setdiff(methods, names(class_weights))) {
        class_weights[m] <- 0
      }
      class_weights
    })
  }

  if (se) {
    if (!inherits(est, "numeric") || is.null(weights$Overall)) {
      message("Standard errors not available.")
      out <- est
    } else {
      methods <- colnames(preds)
      overall_weights <- weights$Overall[methods]

      # Use overall weights to calculate standard error in regression estimations
      std_error <- apply(preds, 1, wtd.sd, w = overall_weights)
      std_error <- qnorm(level) * std_error
      out <- data.frame(
        fit = est,
        lwr = est - std_error,
        upr = est + std_error
      )
    }
  } else {
    out <- est
  }
  if (return_weights) {
    attr(out, "weights") <- weights
  }

  return(out)
}

#' @title Check if an object is a caretStack object
#' @param object an R object
#' @description Check if an object is a caretStack object
#' @export
is.caretStack <- function(object) {
  is(object, "caretStack")
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
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' summary(meta_model)
#' }
summary.caretStack <- function(object, ...) {
  summary(object$ens_model)
}

#' @title Print a caretStack object
#' @description This is a function to print a caretStack.
#' @param x An object of class caretStack
#' @param ... ignored
#' @importFrom stats na.omit
#' @export
#' @examples
#' \dontrun{
#' library("rpart")
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm")
#' print(meta_model)
#' }
print.caretStack <- function(x, ...) {
  base.models <- paste(names(x$models), collapse = ", ")
  cat(sprintf("A %s ensemble of %s base models: %s", x$ens_model$method, length(x$models), base.models))
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
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "rpart", tuneLength = 2)
#' plot(meta_model)
#' }
plot.caretStack <- function(x, ...) {
  plot(x$ens_model, ...)
}

#' @title Comparison dotplot for a caretStack object
#' @description This is a function to make a dotplot from a caretStack.  It uses dotplot from the caret package on all the models in the ensemble, excluding the final ensemble model.  At the moment, this function only works if the ensembling model has the same number of resamples as the component models.
#' @param x An object of class caretStack
#' @param data passed to dotplot
#' @param ... passed to dotplot
#' @importFrom lattice dotplot
#' @importFrom caret resamples
#' @examples
#' \dontrun{
#' set.seed(42)
#' library("rpart")
#' models <- caretList(
#'   x = iris[1:100, 1:2],
#'   y = iris[1:100, 3],
#'   trControl = trainControl(method = "cv"),
#'   methodList = c("rpart", "glm")
#' )
#' meta_model <- caretStack(models, method = "lm", trControl = trainControl(method = "cv"))
#' dotplot.caretStack(meta_model)
#' }
dotplot.caretStack <- function(x, data = NULL, ...) {
  dotplot(resamples(x$models), data = data, ...)
}
