#' @title Check binary classification
#' @description Check that the problem is a binary classification problem
#'
#' @param list_of_models a list of caret models to check
#' @keywords internal
check_binary_classification <- function(list_of_models) {
  if (is.list(list_of_models) && length(list_of_models) > 1L) {
    lapply(list_of_models, function(x) {
      # avoid regression models
      if (is(x, "train") && !is.null(x$pred$obs) && is.factor(x$pred$obs) && nlevels(x$pred$obs) > 2L) {
        stop("caretEnsemble only supports binary classification problems")
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
#' @details Every model in the "library" must be a separate \code{train} object.  For
#' example, if you wish to combine a random forests with several different
#' values of mtry, you must build a model for each value of mtry.  If you
#' use several values of mtry in one train model, (e.g. tuneGrid =
#' expand.grid(.mtry=2:5)), caret will select the best value of mtry
#' before we get a chance to include it in the ensemble.  By default,
#' RMSE is used to ensemble regression models, and AUC is used to ensemble
#' Classification models.  This function does not currently support multi-class
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

#' @title Check if an object is a caretEnsemble object
#' @param object an R object
#' @description Check if an object is a caretEnsemble object
#' @export
is.caretEnsemble <- function(object) {
  is(object, "caretEnsemble")
}

#' @title Extract accuracy metrics from a \code{\link[caret]{train}} model
#' @description Extract the cross-validated accuracy metrics or their SDs from caret.
#' @param x a train object
#' @param metric a character string representing the metric to extract
#' @param return_sd a boolean indicating whether to return the SD of the metric instead of the average.
#' @return A numeric representing the metric desired metric.
#' @export
getMetric <- function(x, metric = NULL, return_sd = FALSE) {
  # Load the metric used by train by default
  if (is.null(metric)) {
    metric <- x$metric
  }

  # Get the metric and its min or max
  stopifnot(metric %in% names(x$results))
  val <- x$results[[metric]]
  idx <- ifelse(x$maximize, which.max(val), which.min(val))

  # If SD, get the SD of the metric instead
  if (return_sd) {
    SD <- x$results[[paste0(metric, "SD")]]
    out <- SD[idx]
  } else {
    out <- val[idx]
  }
  out
}

#' Extract the model accuracy metrics of the individual models in an ensemble object.
#' @param ensemble a caretEnsemble to make predictions from.
#' @param metric a character string representing the metric to extract.
#' If NULL, each model will return the metric it was trained on.
#' If not NULL, the specified metric must be present for EVERY trained model.
#' @importFrom data.table data.table setorderv
extractModelMetrics <- function(ensemble, metric = NULL) {
  stopifnot(is.caretEnsemble(ensemble))
  model_metrics <- data.table(
    model_name = names(ensemble$models),
    metric = sapply(ensemble$models, "[[", "metric"),
    value = sapply(ensemble$models, getMetric, return_sd = FALSE),
    sd = sapply(ensemble$models, getMetric, return_sd = TRUE)
  )
  model_metrics
}

#' @title Summarize the results of caretEnsemble for the user.
#' @description Summarize a caretEnsemble
#' @param object a \code{\link{caretEnsemble}} to make predictions from.
#' @param ... optional additional parameters.
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
#' }
summary.caretEnsemble <- function(object, ...) {
  types <- names(object$models)
  types <- paste(types, collapse = ", ")
  wghts <- coef(object$ens_model$finalModel)
  metric <- object$ens_model$metric
  val <- getMetric(object$ens_model)
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(round(wghts, 4L), collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4L), "\n"))

  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModelMetrics(object), row.names = FALSE)
}

#' @title Calculate the variable importance of variables in a caret model.
#' @description This function wraps the \code{\link[caret]{varImp}} function
#' from the caret package. It returns a \code{\link[data.table]{data.table}} with importances normalized to sum to 1.
#' @param x a \code{\link[caret]{train}} object
#' @param model_name a character string representing the name of the model
#' @param ... additional arguments passed to \code{\link[caret]{varImp}}
#' @return a \code{\link[data.table]{data.table}} with 2 columns: the variables and their importances.
#' @importFrom caret varImp
#' @importFrom data.table data.table
#' @keywords internal
varImpDataTable <- function(x, model_name, ...) {
  imp <- caret::varImp(x, ...)
  imp <- imp[["importance"]]
  imp <- data.table::data.table(
    model_name = model_name,
    var = trimws(gsub("[`()]", "", row.names(imp)), which = "both"),
    imp = imp[["Overall"]] / sum(imp[["Overall"]])
  )
  imp
}

#' @title Calculate the variable importance of variables in a caretEnsemble.
#' @description This function wraps the \code{\link[caret]{varImp}} function in the
#' \code{caret} package to provide a weighted estimate of the importance of
#' variables in the ensembled models in a \code{caretEnsemble} object. Variable
#' importance for each model is calculated and then averaged by the weight of the overall model
#' in the ensembled object.
#' @param object a \code{caretEnsemble} to make predictions from.
#' @param ... additional arguments passed to \code{\link[caret]{varImp}}
#' @importFrom data.table rbindlist dcast.data.table merge.data.table setorderv setcolorder
#' @return A \code{\link[data.table]{data.table}} with one row per variable and one column
#' per model in object
#' @export
varImp.caretEnsemble <- function(object, ...) {
  model_names <- make.names(names(object$models), unique = TRUE, allow_ = TRUE)

  # Individual model importances
  # TODO: varImp.caretList should be a separate function
  model_imp <- mapply(varImpDataTable, object$models, model_names, MoreArgs = list(...), SIMPLIFY = FALSE)
  model_imp <- data.table::rbindlist(model_imp, fill = TRUE, use.names = TRUE)
  model_imp <- data.table::dcast.data.table(model_imp, var ~ model_name, value.var = "imp", fill = 0.0)

  # Overall importance
  ens_imp <- varImpDataTable(object$ens_model, "ensemble")
  ens_imp <- data.table::dcast.data.table(ens_imp, model_name ~ var, value.var = "imp", fill = 0.0)

  # Use overall importance to weight individual model importances
  model_imp_mat <- as.matrix(model_imp[, model_names, with = FALSE])
  ens_imp_mat <- as.matrix(ens_imp[, model_names, with = FALSE])
  overall_imp <- data.table::data.table(
    var = model_imp[["var"]],
    overall = (model_imp_mat %*% t(ens_imp_mat))[, 1L]
  )

  # Merge overall importance with individual model importances
  imp <- data.table::merge.data.table(overall_imp, model_imp, by = "var", all = TRUE)

  # Order and return
  data.table::setorderv(imp, "overall", order = -1L)
  data.table::setcolorder(imp, c("var", "overall", model_names))
  imp
}

#' @title Plot Diagnostics for an caretEnsemble Object
#' @description This function makes a short plot of the performance of the component
#' models of a \code{caretEnsemble} object on the AUC or RMSE metric
#' @param x a \code{caretEnsemble} object
#' @param ... additional arguments to pass to plot
#' @return A plot
#' @importFrom ggplot2 ggplot aes geom_pointrange theme_bw labs geom_hline
#' @importFrom rlang .data
#' @export
#' @method plot caretEnsemble
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("glm", "rpart"))
#' ens <- caretEnsemble(models)
#' plot(ens)
#' }
plot.caretEnsemble <- function(x, ...) {
  dat <- extractModelMetrics(x)
  plt <- ggplot(
    dat, aes(
      x = .data[["model_name"]],
      y = .data[["value"]],
      ymin = .data[["value"]] - .data[["sd"]],
      ymax = .data[["value"]] + .data[["sd"]],
      color = .data[["metric"]]
    )
  ) +
    geom_pointrange() +
    theme_bw() +
    labs(x = "Individual Model Method", y = "Metric Value")

  if (nrow(x$error) > 0L) {
    plt <- plt +
      geom_hline(linetype = 2L, linewidth = 0.2, yintercept = min(x$error[[x$ens_model$metric]]), color = I("red"))
  }
  plt
}

#' @title Extract the best predictions and observations from a train object
#' @description This function extracts the best predictions and observations from a train object
#' and then calculates residuals.  It only uses one class for classification models, by default class 2.
#' @param object a \code{train} object
#' @param show_class_id For classification only: which class level to use for residuals
#' @return a data.table with predictions, observeds, and residuals
#' @importFrom data.table data.table
extractPredObsResid <- function(object, show_class_id = 2L) {
  stopifnot(is(object, "train"))
  type <- object$modelType
  predobs <- caretPredict(object)
  pred <- predobs$pred
  obs <- predobs$obs
  id <- predobs$rowIndex
  if (type == "Regression") {
    pred <- pred[[1L]]
  } else {
    show_class <- levels(object)[show_class_id]
    pred <- pred[[show_class]]
    obs <- as.integer(obs == show_class)
  }
  out <- data.table::data.table(pred, obs, resid = obs - pred, id)
  out
}

#' @title Convenience function for more in-depth diagnostic plots of caretEnsemble objects
#' @description This function provides a more robust series of diagnostic plots
#' for a caretEnsemble object.
#' @param object a \code{caretEnsemble} object
#' @param xvars a vector of the names of x variables to plot against residuals
#' @param show_class_id For classification only: which class level to show on the plot
#' @param ... additional arguments to pass to autoplot
#' @return A grid of diagnostic plots. Top left is the range of the performance
#' metric across each component model along with its standard deviation. Top right
#' is the residuals from the ensembled model plotted against fitted values.
#' Middle left is a bar graph of the weights of the component models. Middle
#' right is the disagreement in the residuals of the component models (unweighted)
#' across the fitted values. Bottom left and bottom right are the plots of the
#' residuals against two random or user specified variables.
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_bar geom_linerange
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous labs theme_bw autoplot
#' @importFrom data.table data.table set rbindlist setkeyv .SD
#' @importFrom rlang .data
#' @importFrom gridExtra grid.arrange
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
  stopifnot(is(object, "caretEnsemble"))
  ensemble_data <- extractPredObsResid(object$ens_model, show_class_id = show_class_id)
  data.table::setkeyv(ensemble_data, "id")

  # Performance metrics by model
  g1 <- plot(object) + labs(title = "Metric and SD For Component Models")

  # Residuals vs Fitted
  # Disable the object usage linter in here — it raises false positives for .SD and .data
  g2 <- ggplot2::ggplot(ensemble_data, ggplot2::aes(.data[["pred"]], .data[["resid"]])) +
    geom_point() +
    geom_smooth(se = FALSE) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residual") +
    labs(title = "Residuals vs Fitted") +
    theme_bw()

  # Model Weights
  wghtFrame <- as.data.frame(coef(object$ens_model$finalModel))
  wghtFrame$method <- row.names(wghtFrame)
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
  sub_model_data <- data.table::rbindlist(sub_model_data)
  sub_model_summary <- sub_model_data[, list(
    ymin = min(.SD[["resid"]]),
    ymax = max(.SD[["resid"]]),
    yavg = median(.SD[["resid"]]),
    yhat = .SD[["pred"]][1L]
  ), by = "id"]
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
  data.table::set(x_data, j = "id", value = seq_len(nrow(x_data)))
  plotdf <- merge(ensemble_data, x_data, by = "id")
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
  suppressMessages(gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2L))
}
