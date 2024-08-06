#' @title Check binary classification
#' @description Check that the problem is a binary classification problem
#'
#' @param list_of_models a list of caret models to check
#' @keywords internal
check_binary_classification <- function(list_of_models) {
  if (is.list(list_of_models) && length(list_of_models) > 1L) {
    lapply(list_of_models, function(x) {
      # avoid regression models
      if (methods::is(x, "train") && !is.null(x$pred$obs) && is.factor(x$pred$obs) && nlevels(x$pred$obs) > 2L) {
        stop("caretEnsemble only supports binary classification problems", call. = FALSE)
      }
    })
  }
  invisible(NULL)
}

#' @title Combine several predictive models via weights
#'
#' @description Find a good linear combination of several classification or regression models,
#' using linear regression.
#'
#' @details Every model in the "library" must be a separate \code{train} object. For
#' example, if you wish to combine a random forests with several different
#' values of mtry, you must build a model for each value of mtry. If you
#' use several values of mtry in one train model, (e.g. tuneGrid =
#' expand.grid(.mtry=2:5)), caret will select the best value of mtry
#' before we get a chance to include it in the ensemble. By default,
#' RMSE is used to ensemble regression models, and AUC is used to ensemble
#' Classification models. This function does not currently support multi-class
#' problems
#' @note Currently when missing values are present in the training data, weights
#' are calculated using only observations which are complete across all models
#' in the library.The optimizer ignores missing values and calculates the weights with the
#' observations and predictions available for each model separately. If each of the
#' models has a different pattern of missingness in the predictors, then the resulting
#' ensemble weights may be biased and the function issues a message.
#' @param all.models an object of class caretList
#' @param ... additional arguments to pass to the optimization function
#' @return a \code{\link{caretEnsemble}} object
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
#' }
caretEnsemble <- function(all.models, ...) {
  check_binary_classification(all.models)
  out <- caretStack(all.models, method = "glm", ...)
  class(out) <- c("caretEnsemble", "caretStack")
  out
}
