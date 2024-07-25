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
  val <- getMetric.train(object$ens_model)
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(round(wghts, 4L), collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4L), "\n"))

  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModRes(object), row.names = FALSE)
}

#' Extract the model accuracy metrics of the individual models in an ensemble object.
#' @param ensemble a caretEnsemble to make predictions from.
extractModRes <- function(ensemble) {
  stopifnot(is.caretEnsemble(ensemble))
  model_methods <- names(ensemble$models)
  metric <- ensemble$ens_model$metric
  modRes <- data.frame(
    method = model_methods,
    metric = unlist(
      lapply(
        ensemble$models,
        getMetric.train,
        metric = metric
      )
    ),
    metricSD = unlist(
      lapply(
        ensemble$models,
        getMetricSD.train,
        metric = metric
      )
    ),
    stringsAsFactors = FALSE
  )
  names(modRes)[2L:3L] <- c(metric, paste0(metric, "SD"))
  modRes
}

#' Extract accuracy metrics from a model
#' @param x a train object
#' @param metric which metric to get
#' @param ... passed through
#' @rdname metrics
#' @export
getMetric <- function(x, metric, ...) {
  UseMethod("getMetric")
}

#' Extract accuracy metrics SDs from a model
#' @rdname metrics
#' @export
getMetricSD <- function(x, metric, ...) {
  UseMethod("getMetricSD")
}

#' Extract a model accuracy metric from a \code{\link[caret]{train}} object.
#' @return A numeric representing the metric desired metric.
#' @rdname metrics
#' @export
getMetric.train <- function(x, metric = NULL, ...) {
  if (is.null(metric)) {
    metric <- x$metric
  }
  stopifnot(metric %in% names(x$results))
  val <- x$results[[metric]]
  val <- ifelse(x$maximize, max(val, na.rm = TRUE), min(val, na.rm = TRUE))
  val
}

#' Extract the standard deviation from resamples for an accuracy metric from a model object.
#' @rdname metrics
#' @export
getMetricSD.train <- function(x, metric, ...) {
  stopifnot(metric %in% names(x$results))
  val <- x$results[[metric]]
  SD <- x$results[[paste0(metric, "SD")]]
  idx <- ifelse(x$maximize, which.max(val), which.min(val))
  SD[idx]
}

#' @title Calculate the variable importance of variables in a caretEnsemble.
#' @description This function wraps the \code{\link[caret]{varImp}} function in the
#' \code{caret} package to provide a weighted estimate of the importance of
#' variables in the ensembled models in a \code{caretEnsemble} object. Variable
#' importance for each model is calculated and then averaged by the weight of the overall model
#' in the ensembled object.
#' @param object a \code{caretEnsemble} to make predictions from.
#' @param ... other arguments to be passed to varImp
#' @return A \code{\link{data.frame}} with one row per variable and one column
#' per model in object
#' @importFrom digest digest
#' @importFrom caret varImp
#' @export
varImp.caretEnsemble <- function(object, ...) {
  # Extract and formal individual model importances
  # Todo, clean up this code!
  coef_importance <- lapply(object$models, caret::varImp)
  coef_importance <- lapply(coef_importance, clean_varImp)

  # Convert to data.frame
  dat <- varImpFrame(coef_importance)
  dat[is.na(dat)] <- 0L
  names(dat) <- make.names(names(coef_importance))

  # Scale the importances
  norm_to_100 <- function(d) d / sum(d) * 100.0
  dat <- apply(dat, 2L, norm_to_100)

  # Calculate overall importance
  model_weights <- coef(object$ens_model$finalModel)
  # In the case of 2 classes each method will
  # have only one coef associated.
  # The names of the weights keep the order of the
  # models in the ensemble
  names(model_weights) <- names(object$models)
  model_weights <- model_weights[names(model_weights) %in% names(coef_importance)]
  model_weights <- abs(model_weights)
  overall <- norm_to_100(apply(dat, 1L, weighted.mean, w = model_weights))
  dat <- data.frame(overall = overall, dat)

  # Order by overall importance
  dat <- dat[order(dat[["overall"]]), ]

  dat
}

#' @keywords internal
# This function only gets called once, in varImp.caretEnsemble
clean_varImp <- function(x) {
  names(x$importance)[1L] <- "Overall"
  x$importance <- x$importance[, "Overall", drop = FALSE]
  x$importance
}

#' @keywords internal
# This function only gets called once, in varImp.caretEnsemble
varImpFrame <- function(x) {
  dat <- do.call(rbind.data.frame, x)
  dat <- dat[!duplicated(lapply(dat, summary))]

  # Parse frame
  dat$id <- row.names(dat)
  dat$model <- sub("\\.[^\n]*", "", dat$id)
  dat$var <- sub("^[^.]*", "", dat$id)
  dat$var <- substr(dat$var, 2L, nchar(dat$var))

  # Parse intercept variables
  dat$var[grep("Inter", dat$var, fixed = TRUE)] <- "Intercept"
  dat$id <- NULL
  row.names(dat) <- NULL
  dat <- reshape(dat,
    direction = "wide", v.names = "Overall",
    idvar = "var", timevar = "model"
  )
  row.names(dat) <- dat[, 1L]
  dat[, -1L]
}

#' @title Plot Diagnostics for an caretEnsemble Object
#' @description This function makes a short plot of the performance of the component
#' models of a \code{caretEnsemble} object on the AUC or RMSE metric
#' @param x a \code{caretEnsemble} object
#' @param ... additional arguments to pass to plot
#' @return A plot
#' @importFrom ggplot2 ggplot aes geom_pointrange theme_bw labs geom_hline
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
  dat <- extractModRes(x)
  metricLab <- x$ens_model$metric
  dat$metric <- dat[[metricLab]]
  dat$metricSD <- dat[[paste0(metricLab, "SD")]]
  plt <- ggplot(
    dat, aes(
      x = method, y = metric,
      ymin = metric - metricSD,
      ymax = metric + metricSD
    )
  ) +
    geom_pointrange() +
    theme_bw() +
    labs(x = "Individual Model Method", y = metricLab)

  if (nrow(x$error) > 0L) {
    plt <- plt +
      geom_hline(linetype = 2L, linewidth = 0.2, yintercept = min(x$error[[metricLab]]), color = I("red"))
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
  predobs <- extractBestPredsAndObs(list(object))
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
#' @importFrom data.table data.table set rbindlist setkey .SD
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
  data.table::setkey(ensemble_data, id)

  # Performance metrics by model
  g1 <- plot(object) + labs(title = "Metric and SD For Component Models")

  # Residuals vs Fitted
  # Disable the object usage linter in here — it raises false positives for .SD and .data
  # nolint start: object_usage_linter
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
    yhat = .SD[["pred"]][1]
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
  x_data <- data.table::data.table(object$models[[1]]$trainingData)
  if (is.null(xvars)) {
    xvars <- names(x_data)
    xvars <- setdiff(xvars, c(".outcome", ".weights", "(Intercept)"))
    xvars <- sample(xvars, 2L)
  }
  data.table::set(x_data, j = "id", value = seq_len(nrow(x_data)))
  plotdf <- merge(ensemble_data, x_data, by = "id")
  g5 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[1]]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous(xvars[1L]) +
    ggplot2::scale_y_continuous("Residuals") +
    ggplot2::labs(title = paste0("Residuals Against ", xvars[1L])) +
    ggplot2::theme_bw()
  g6 <- ggplot2::ggplot(plotdf, ggplot2::aes(.data[[xvars[2L]]], .data[["resid"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::scale_x_continuous(xvars[2]) +
    ggplot2::scale_y_continuous("Residuals") +
    ggplot2::labs(title = paste0("Residuals Against ", xvars[2L])) +
    ggplot2::theme_bw()
  # nolint end: object_usage_linter
  suppressMessages(gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2L))
}
