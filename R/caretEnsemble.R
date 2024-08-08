#' @title Combine several predictive models via weights
#'
#' @description Find a greedy, positive only linear combination of several \code{\link[caret]{train}} objects
#'
#' @details greedyMSE works well when you want an ensemble that will never be worse than any
#' single model in the dataset. In the worst case scenario, it will select the single
#' best model, if none of them can be ensembled to improve the overall score. It will
#' also never assign any model a negative coefficient, which can help avoid
#' unintuitive cases at prediction time (e.g. if the correlations between
#' predictors breaks down on new data, negative coefficients can lead to bad results).
#'
#' @note Every model in the "library" must be a separate \code{train} object. For
#' example, if you wish to combine a random forests with several different
#' values of mtry, you must build a model for each value of mtry. If you
#' use several values of mtry in one train model, (e.g. tuneGrid =
#' expand.grid(.mtry=2:5)), caret will select the best value of mtry
#' before we get a chance to include it in the ensemble. By default,
#' RMSE is used to ensemble regression models, and AUC is used to ensemble
#' Classification models. This function does not currently support multi-class
#' problems
#' @param all.models an object of class caretList
#' @param excluded_class_id The integer level to exclude from binary classification or multiclass problems.
#' By default no classes are excluded, as the greedy optimizer requires all classes because it cannot
#' use negative coefficients.
#' @param tuneLength The size of the grid to search for tuning the model. Defaults to 1, as
#' the only parameter to optimize is the number of iterations, and the default of 100 works well.
#' @param ... additional arguments to pass caret::train
#' @return a \code{\link{caretEnsemble}} object
#' @export
#' @examples
#' set.seed(42)
#' models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
caretEnsemble <- function(
    all.models,
    excluded_class_id = 0L,
    tuneLength = 1L,
    ...) {
  out <- caretStack(
    all.models,
    excluded_class_id = excluded_class_id,
    tuneLength = tuneLength,
    method = greedyMSE_caret(),
    ...
  )
  class(out) <- c("caretEnsemble", "caretStack")
  out
}
