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

#' @title Check if an object is a caretEnsemble object
#' @param object an R object
#' @description Check if an object is a caretEnsemble object
#' @export
is.caretEnsemble <- function(object) {
  methods::is(object, "caretEnsemble")
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
extractModelMetrics <- function(ensemble, metric = NULL) {
  stopifnot(is.caretEnsemble(ensemble))
  model_metrics <- data.table::data.table(
    model_name = names(ensemble$models),
    metric = vapply(ensemble$models, "[[", character(1L), "metric"),
    value = vapply(ensemble$models, getMetric, numeric(1L), return_sd = FALSE),
    sd = vapply(ensemble$models, getMetric, numeric(1L), return_sd = TRUE)
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
  types <- toString(types)
  wghts <- stats::coef(object$ens_model$finalModel)
  metric <- object$ens_model$metric
  val <- getMetric(object$ens_model)
  cat("The following models were ensembled:", types, " \n")
  cat("They were weighted: \n")
  cat(toString(round(wghts, 4L)), "\n")
  cat("The resulting ", metric, "is:", round(val, 4L), "\n")

  # Add code to compare ensemble to individual models
  cat("The fit for each individual model on the", metric, "is: \n")
  print(extractModelMetrics(object), row.names = FALSE)
}

#' @title Calculate the variable importance of variables in a caret model.
#' @description This function wraps the \code{\link[caret]{varImp}} function
#' from the caret package. It returns a \code{\link[data.table]{data.table}} with importances normalized to sum to 1.
#' @param x a \code{\link[caret]{train}} object
#' @param model_name a character string representing the name of the model
#' @param ... additional arguments passed to \code{\link[caret]{varImp}}
#' @return a \code{\link[data.table]{data.table}} with 2 columns: the variables and their importances.
#' @keywords internal
varImpDataTable <- function(x, model_name, ...) {
  imp <- caret::varImp(x, ...)
  imp <- imp[["importance"]]
  # Normalize to sum to 1, by class or overall
  imp_by_class <- data.table::as.data.table(lapply(imp, function(x) x / sum(x)))
  imp <- data.table::data.table(
    model_name = model_name,
    var = trimws(gsub("[`()]", "", row.names(imp)), which = "both"),
    imp_by_class
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
#' @return A \code{\link[data.table]{data.table}} with one row per variable and one column
#' per model in object
#' @importFrom caret varImp
#' @method varImp caretEnsemble
#' @export
varImp.caretEnsemble <- function(object, ...) {
  model_names <- make.names(names(object$models), unique = TRUE, allow_ = TRUE)

  # Individual model importances
  # TODO: varImp.caretList should be a separate function
  model_imp <- Map(varImpDataTable, object$models, model_names, MoreArgs = list(...))
  model_imp <- data.table::rbindlist(model_imp, fill = TRUE, use.names = TRUE)
  model_imp <- data.table::dcast.data.table(model_imp, var ~ model_name, value.var = "Overall", fill = 0.0)

  # Overall importance
  ens_imp <- varImpDataTable(object$ens_model, "ensemble")
  ens_imp <- data.table::dcast.data.table(ens_imp, model_name ~ var, value.var = "Overall", fill = 0.0)

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
#' @importFrom graphics plot
#' @method plot caretEnsemble
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50, 1:2], iris[1:50, 3], methodList = c("glm", "rpart"))
#' ens <- caretEnsemble(models)
#' plot(ens)
#' }
plot.caretEnsemble <- function(x, ...) {
  dat <- extractModelMetrics(x)
  plt <- ggplot2::ggplot(
    dat, ggplot2::aes(
      x = .data[["model_name"]],
      y = .data[["value"]],
      ymin = .data[["value"]] - .data[["sd"]],
      ymax = .data[["value"]] + .data[["sd"]],
      color = .data[["metric"]]
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Individual Model Method", y = "Metric Value")

  if (nrow(x$error) > 0L) {
    plt <- plt +
      ggplot2::geom_hline(
        linetype = 2L,
        linewidth = 0.2,
        yintercept = min(x$error[[x$ens_model$metric]]),
        color = I("red")
      )
  }
  plt
}

#' @title Extract the best predictions and observations from a train object
#' @description This function extracts the best predictions and observations from a train object
#' and then calculates residuals. It only uses one class for classification models, by default class 2.
#' @param object a \code{train} object
#' @param show_class_id For classification only: which class level to use for residuals
#' @return a data.table::data.table with predictions, observeds, and residuals
extractPredObsResid <- function(object, show_class_id = 2L) {
  stopifnot(
    methods::is(object, "train"),
    is.data.frame(object$pred)
  )
  keep_cols <- c("pred", "obs", "rowIndex")
  type <- object$modelType
  predobs <- data.table::data.table(object$pred)
  if (type == "Classification") {
    show_class <- levels(object)[show_class_id]
    data.table::set(predobs, j = "pred", value = predobs[[show_class]])
    data.table::set(predobs, j = "obs", value = as.integer(predobs[["obs"]] == show_class))
  }
  predobs <- predobs[, keep_cols, with = FALSE]
  data.table::setkeyv(predobs, "rowIndex")
  predobs <- predobs[, lapply(.SD, mean), by = "rowIndex"]
  r <- predobs[["obs"]] - predobs[["pred"]]
  data.table::set(predobs, j = "resid", value = r)
  data.table::setorderv(predobs, "rowIndex")
  predobs
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
#' @importFrom ggplot2 autoplot
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
  sub_model_data <- data.table::rbindlist(sub_model_data)
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
  suppressWarnings(suppressMessages(gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2L)))
}
