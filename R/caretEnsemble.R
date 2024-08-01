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

#' @title Convenience function for more in-depth diagnostic plots of caretEnsemble objects
#' @description This function provides a more robust series of diagnostic plots
#' for a caretEnsemble object.
#' @param object a \code{caretEnsemble} object
#' @param xvars a vector of the names of x variables to plot against residuals
#' @param show_class_id For classification only: which class level to show on the plot
#' @param ... ignored
#' @return A grid of diagnostic plots. Top left is the range of the performance
#' metric across each component model along with its standard deviation. Top right
#' is the residuals from the ensembled model plotted against fitted values.
#' Middle left is a bar graph of the weights of the component models. Middle
#' right is the disagreement in the residuals of the component models (unweighted)
#' across the fitted values. Bottom left and bottom right are the plots of the
#' residuals against two random or user specified variables.
#' @importFrom ggplot2 autoplot
#' @importFrom patchwork plot_layout
#' @method autoplot caretEnsemble
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' data(models.reg)
#' ens <- caretEnsemble(
#'   models.reg,
#'   trControl = caret::trainControl(
#'     method = "cv", savePredictions = "final"
#'   )
#' )
#' suppressWarnings(autoplot(ens))
#' }
autoplot.caretEnsemble <- function(object, xvars = NULL, show_class_id = 2L, ...) {
  stopifnot(methods::is(object, "caretEnsemble"))
  ensemble_data <- extractPredObsResid(object$ens_model, show_class_id = show_class_id)

  # Performance metrics by model
  g1 <- plot(object) + ggplot2::labs(title = "Metric and SD For Component Models")

  # Residuals vs Fitted
  # Disable the object usage linter in here — it raises false positives for .SD and .data
  g2 <- ggplot2::ggplot(ensemble_data, ggplot2::aes(.data[["pred"]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous("Fitted Values") +
    ggplot2::scale_y_continuous("Residual") +
    ggplot2::labs(title = "Residuals vs Fitted") +
    ggplot2::theme_bw()

  # Model Weights
  wghtFrame <- data.table::as.data.table(stats::coef(object[["ens_model"]][["finalModel"]]))
  data.table::set(wghtFrame, j = "method", value = row.names(wghtFrame))
  names(wghtFrame) <- c("weights", "method")
  g3 <- ggplot2::ggplot(wghtFrame, ggplot2::aes(.data[["method"]], .data[["weights"]])) +
    ggplot2::geom_bar(stat = "identity", fill = I("gray50"), color = I("black")) +
    ggplot2::labs(title = "Model Weights", x = "Method", y = "Weights") +
    ggplot2::theme_bw()

  # Disagreement in sub-model residuals
  sub_model_data <- lapply(object$models, extractPredObsResid, show_class_id = show_class_id)
  for (model_name in names(sub_model_data)) {
    data.table::set(sub_model_data[[model_name]], j = "model", value = model_name)
  }
  sub_model_data <- data.table::rbindlist(sub_model_data, use.names = TRUE, fill = TRUE)
  sub_model_summary <- sub_model_data[, list(
    ymin = min(.SD[["resid"]]),
    ymax = max(.SD[["resid"]]),
    yavg = stats::median(.SD[["resid"]]),
    yhat = .SD[["pred"]][1L]
  ), by = "rowIndex"]
  g4 <- ggplot2::ggplot(sub_model_summary, ggplot2::aes(
    x = .data[["yhat"]],
    y = .data[["yavg"]]
  )) +
    ggplot2::geom_linerange(alpha = I(0.5), ggplot2::aes(
      ymin = .data[["ymin"]],
      ymax = .data[["ymax"]]
    )) +
    ggplot2::geom_point(size = I(3L), alpha = I(0.8)) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(
      method = "lm", se = FALSE,
      linewidth = I(1.1), color = I("red"), linetype = 2L
    ) +
    ggplot2::labs(
      x = "Fitted Values", y = "Range of Resid.",
      title = "Model Disagreement Across Fitted Values"
    )

  # Residuals vs X variables
  x_data <- data.table::data.table(object$models[[1L]]$trainingData)
  if (is.null(xvars)) {
    xvars <- names(x_data)
    xvars <- setdiff(xvars, c(".outcome", ".weights", "(Intercept)"))
    xvars <- sample(xvars, 2L)
  }
  data.table::set(x_data, j = "rowIndex", value = seq_len(nrow(x_data)))
  plotdf <- merge(ensemble_data, x_data, by = "rowIndex")
  g5 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[1L]]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous(xvars[1L]) +
    ggplot2::scale_y_continuous("Residuals") +
    ggplot2::labs(title = paste0("Residuals Against ", xvars[1L])) +
    ggplot2::theme_bw()
  g6 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[2L]]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous(xvars[2L]) +
    ggplot2::scale_y_continuous("Residuals") +
    ggplot2::labs(title = paste0("Residuals Against ", xvars[2L])) +
    ggplot2::theme_bw()
  out <- g1 + g2 / (g3 + g4) / (g5 + g6)
  out
}
