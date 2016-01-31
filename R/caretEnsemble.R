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
#' models <- caretList(iris[1:50,1:2], iris[1:50,3], methodList=c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
#' }
caretEnsemble <- function(all.models, ...){
  out <- caretStack(all.models, method="glm", ...)
  class(out) <- c("caretEnsemble", "caretStack")
  return(out)
}

#' @title Check if an object is a caretEnsemble object
#' @param object an R object
#' @description Check if an object is a caretEnsemble object
#' @export
is.caretEnsemble <- function(object){
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
#' models <- caretList(iris[1:50,1:2], iris[1:50,3], methodList=c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
#' }
summary.caretEnsemble <- function(object, ...){
  types <- names(object$models)
  types <- paste(types, collapse = ", ")
  wghts <- coef(object$ens_model$finalModel)
  metric <- object$ens_model$metric
  val <- getMetric.train(object$ens_model)
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(round(wghts, 4), collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4), "\n"))

  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModRes(object), row.names = FALSE)
}

#' Extract the model accuracy metrics of the individual models in an ensemble object.
#' @param ensemble a caretEnsemble to make predictions from.
extractModRes <- function(ensemble){
  stopifnot(is.caretEnsemble(ensemble))
  methods <- names(ensemble$models)
  metric <- ensemble$ens_model$metric
  modRes <- data.frame(
    method = methods,
    metric = unlist(
      lapply(
        ensemble$models,
        getMetric.train,
        metric = metric)),
    metricSD = unlist(
      lapply(
        ensemble$models,
        getMetricSD.train,
        metric = metric)),
    stringsAsFactors = FALSE)
  names(modRes)[2:3] <- c(metric, paste0(metric, "SD"))
  return(modRes)
}

#' Extract accuracy metrics from a model
#' @param x a train object
#' @param metric which metric to get
#' @param ... passed through
#' @rdname metrics
#' @export
getMetric <- function(x, metric, ...){
  UseMethod("getMetric")
}

#' Extract accuracy metrics SDs from a model
#' @rdname metrics
#' @export
getMetricSD <- function(x, metric, ...){
  UseMethod("getMetricSD")
}

#' Extract a model accuracy metric from a \code{\link{train}} object.
#' @return A numeric representing the metric desired metric.
#' @rdname metrics
#' @export
getMetric.train <- function(x, metric=NULL, ...){
  if(is.null(metric)){
    metric <- x$metric
  }
  stopifnot(metric %in% names(x$results))
  val <- x$results[[metric]]
  val <- ifelse(x$maximize, max(val, na.rm=TRUE), min(val, na.rm=TRUE))
  val
}

#' Extract the standard deviation from resamples for an accuracy metric from a model object.
#' @rdname metrics
#' @export
getMetricSD.train <- function(x, metric, ...){
  stopifnot(metric %in% names(x$results))
  val <- x$results[[metric]]
  SD <- x$results[[paste0(metric, "SD")]]
  idx <- ifelse(x$maximize, which.max(val), which.min(val))
  SD[idx]
}

#' @keywords internal
matchBestTune <- function(out, bt){
  nams <- names(attributes(out)$dimnames)
  nams <- nams[nams %in% names(bt)]
  tmp <- c()
  for(i in length(nams)){
    tmp.t <- out[attributes(out)$dimnames[[nams[[i]]]] == as.character(bt[, nams[i]])]
    tmp <- c(tmp, tmp.t)
  }
  return(tmp)
}

#' @title Calculate the variable importance of variables in a caretEnsemble.
#' @description This function wraps the \code{\link{varImp}} function in the
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
varImp.caretEnsemble <- function(object, ...){

  #Extract and formal individual model importances
  #Todo, clean up this code!
  a <- lapply(object$models, varImp)
  a <- lapply(a, clean_varImp)

  #Convert to data.frame
  dat <- varImpFrame(a)
  dat[is.na(dat)] <- 0
  names(dat) <- make.names(names(a))

  #Scale the importances
  norm_to_100 <- function(d) d / sum(d) * 100
  dat[] <- lapply(dat, norm_to_100)

  #Calculated the overall importance
  weights <- coef(object$ens_model$finalModel)
  weights <- weights[names(weights) %in% names(a)]
  weights <- abs(weights)
  overall <- apply(dat, 1, weighted.mean, w=weights)
  overall <- norm_to_100(overall)
  dat <- data.frame(
    overall = overall,
    dat
  )

  #Order, and return
  dat <- dat[order(dat[["overall"]]), ]
  return(dat)
}

#' @keywords internal
# This function only gets called once, in varImp.caretEnsemble
clean_varImp <- function(x){
  names(x$importance)[1] <- "Overall"
  x$importance <- x$importance[, "Overall", drop=FALSE]
  return(x$importance)
}

#' @keywords internal
# This function only gets called once, in varImp.caretEnsemble
varImpFrame <- function(x){

  dat <- do.call(rbind.data.frame, x)
  dat <- dat[!duplicated(lapply(dat, summary))]

  # Parse frame
  dat$id <- row.names(dat)
  dat$model <- sub("\\.[^\n]*", "", dat$id)
  dat$var <- sub("^[^.]*", "", dat$id)
  dat$var <- substr(dat$var, 2, nchar(dat$var))

  # Parse intercept variables
  dat$var[grep("Inter", dat$var)] <- "Intercept"
  dat$id <- NULL
  row.names(dat) <- NULL
  dat <- reshape(dat, direction = "wide", v.names="Overall",
                 idvar = "var", timevar = "model")
  row.names(dat) <- dat[, 1]
  return(dat[, -1])
}

#' @title Calculate the residuals from a caretEnsemble.
#' @description This function calculates raw residuals for both regression and
#' classification \code{caretEnsemble} objects.
#' @param object a \code{caretEnsemble} to make predictions from.
#' @param ... other arguments to be passed to residuals
#' @return A numeric of the residuals.
residuals.caretEnsemble <- function(object, ...){
  if(is.null(object$modelType)){
    object$modelType <- extractModelTypes(object$models)[1]
  }
  if(object$modelType == "Regression"){
    yhat <- predict(object)
    y <- object$models[[1]]$trainingData$.outcome
    resid <- y - yhat
    return(resid)
  } else if(object$modelType == "Classification"){
    yhat <- predict(object, type="prob")
    y <- as.character(object$models[[1]]$trainingData$.outcome)
    z <- table(y)
    prevOutcome <- names(z)[z == max(z)]
    y <- ifelse(y == prevOutcome, 0, 1)
    resid <- y - yhat
    return(resid)
  }
}

#' @title Calculate the residuals from all component models of a caretEnsemble.
#' @description This function calculates raw residuals for both regression and
#' classification \code{train} objects within a \code{\link{caretEnsemble}}.
#' @param object a \code{caretEnsemble} to make predictions from.
#' @param ... other arguments to be passed to residuals
#' @return A data.frame in the long format with columns for the model method,
#' the observation id, yhat for the fitted values, resid for the residuals, and
#' y for the observed value.
multiResiduals <- function(object, ...){
  stopifnot(is(object$models, "caretList"))
  modtype <- extractModelTypes(object$models)
  preds <- predict(object$models, ...)
  if(modtype == "Regression"){
    y <- object$models[[1]]$trainingData$.outcome
  } else if(modtype == "Classification"){
    y <- as.character(object$models[[1]]$trainingData$.outcome)
    z <- table(y)
    prevOutcome <- names(z)[z == max(z)]
    y <- ifelse(y == prevOutcome, 0, 1)
  }
  resid <- y - preds
  preds <- as.data.frame(preds)
  resid <- as.data.frame(resid)
  resid <- reshape(resid, direction = "long", varying = names(resid),
                    v.names = "resid", timevar = "method", times = names(resid))
  preds <- reshape(preds, direction = "long", varying = names(preds),
                   v.names = "yhat", timevar = "method", times = names(preds))
  out <- merge(preds, resid)
  out$y <- out$yhat + out$resid
  return(out)
}

#' @title Supplement the data fitted to a caret ensemble model with model fit statistics
#' @description This function constructs a dataframe consisting of the outcome,
#' all of the predictors used in any of the models ensembled in a \code{caretEnsemble}
#' object, and some model fit statistics.
#' @param model a \code{caretEnsemble} to extract predictors from
#' @param data a data set, defaults to the data used to fit the model
#' @param ... additional arguments to pass to fortify
#' @return The original data with extra columns for fitted values and residuals
fortify <- function(model, data = NULL, ...){
  data <- extractModFrame(model)
  data$y <- model$models[[1]]$trainingData$.outcome
  if(class(data$y) != "numeric"){
    data$y <- as.character(data$y)
    z <- table(data$y)
    prevOutcome <- names(z)[z == max(z)]
    data$y <- ifelse(data$y == prevOutcome, 0, 1)
    data$y <- as.numeric(data$y)
  }
  if(model$ens_model$modelType == "Classification"){
    data$.fitted <- predict(model, type="prob")
  } else if(model$ens_model$modelType == "Regression"){
    data$.fitted <- predict(model)
  }else{
    stop(paste("Uknown model type", model$ens_model$modelType))
  }

  data$.resid <- residuals(model)
  return(data)
}

#' @title Extract a dataframe of all predictors used in a caretEnsemble object.
#' @description This function constructs a dataframe consisting of the outcome
#' and all of the predictors used in any of the models ensembled in a \code{caretEnsemble}
#' object.
#' @param model a \code{caretEnsemble} to extract predictors from
#' @return A data.frame combining all of the variables used across all models.
#' @importFrom digest digest
extractModFrame <- function(model){
  datList <- vector("list", length = length(model$models))
  for(i in 1: length(model$models)){
    datList[[i]] <- model$models[[i]]$trainingData
  }
  modelFrame <- do.call(cbind, datList)
  modelFrame <- modelFrame[!duplicated(lapply(modelFrame, digest))]
  return(modelFrame)
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
#' models <- caretList(iris[1:50,1:2], iris[1:50,3], methodList=c("glm", "rpart"))
#' ens <- caretEnsemble(models)
#' plot(ens)
#' }
plot.caretEnsemble <- function(x, ...){

  # TODO: USE OUT OF SAMPLE RESIDUALS

  dat <- extractModRes(x)
  metricLab <- x$ens_model$metric
  dat$metric <- dat[[metricLab]]
  dat$metricSD <- dat[[paste0(metricLab, "SD")]]
  plt <- ggplot(
    dat, aes(
      x = method, y = metric,
      ymin = metric - metricSD,
      ymax = metric + metricSD)) +
    geom_pointrange() +
    theme_bw() + labs(x = "Individual Model Method", y = metricLab)

  if(nrow(x$error) > 0){
    plt <- plt +
    geom_hline(linetype = 2, size = 0.2, yintercept = min(x$error[[metricLab]]), color = I("red"), size = I(1.1))
  }
  return(plt)
}



#' @title Convenience function for more in-depth diagnostic plots of caretEnsemble objects
#' @description This function provides a more robust series of diagnostic plots
#' for a caretEnsemble object.
#' @param object a \code{caretEnsemble} object
#' @param which an integer index for which of the plots to print.  DOES NOTHING.
#' @param mfrow an integer vector of length 2 specifying the number of rows and columns for plots
#' @param xvars a vector of the names of x variables to plot against residuals
#' @param ... additional arguments to pass to autoplot
#' @return A grid of diagnostic plots. Top left is the range of the performance
#' metric across each component model along with its standard deviation. Top right
#' is the residuals from the ensembled model plotted against fitted values.
#' Middle left is a bar graph of the weights of the component models. Middle
#' right is the disagreement in the residuals of the component models (unweighted)
#' across the fitted values. Bottom left and bottom right are the plots of the
#' residuals against two random or user specified variables.
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_x_continuous scale_y_continuous theme_bw geom_bar labs geom_linerange aes_string
#' @importFrom plyr ddply summarize .
#' @importFrom gridExtra grid.arrange
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(
#'  iris[1:50,1:2],
#'  iris[1:50,3],
#'  trControl=trainControl(method="cv"),
#'  methodList=c("glm", "rpart"))
#' ens <- caretEnsemble(models)
#' autoplot(ens)
#' }
autoplot <- function(object, which = c(1:6), mfrow = c(3, 2),
                                   xvars = NULL, ...){
  plotdf <- suppressMessages(fortify(object))
  g1 <- plot(object) + labs(title = "Metric and SD For Component Models")
  wghtFrame <- as.data.frame(coef(object$ens_model$finalModel))
  wghtFrame$method <- row.names(wghtFrame)
  names(wghtFrame) <- c("weights", "method")
  g2 <- ggplot(plotdf, aes(.fitted, .resid)) + geom_point() + geom_smooth(se = FALSE) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residual") + labs(title = "Residuals vs Fitted") +
    theme_bw()
  g3 <- ggplot(wghtFrame, aes(method, weights)) +
    geom_bar(stat = "identity", fill = I("gray50"), color = I("black")) +
    labs(title = "Model Weights", x = "Method", y = "Weights") + theme_bw()
  if(missing(xvars)){
    xvars <- names(plotdf)[!names(plotdf) %in% c("(Intercept)", ".outcome", "y",
                                                 ".fitted", ".resid")]
    xvars <- sample(xvars, 2)
  }
  # TODO: Insert checks for length of xvars here
  residOut <- multiResiduals(object)
  zed <- ddply(residOut, .(id), summarize,
               ymin = min(resid),
               ymax = max(resid),
               yavg = median(resid),
               yhat = yhat[1])
  g4 <- ggplot(zed, aes(x = yhat, ymin = ymin, ymax = ymax, y = yavg)) +
    geom_linerange(alpha = I(0.5)) + geom_point(size = I(3), alpha = I(0.8)) +
    theme_bw() + geom_smooth(method = "lm", se = FALSE,
                             size = I(1.1), color = I("red"), linetype = 2) +
    labs(x = "Fitted Values", y = "Range of Resid.",
         title = "Model Disagreement Across Fitted Values")
  g5 <- ggplot(plotdf, aes_string(xvars[1], ".resid")) + geom_point() +
    geom_smooth(se = FALSE) + scale_x_continuous(xvars[1]) +
    scale_y_continuous("Residuals") +
    labs(title = paste0("Residuals Against ", xvars[1])) + theme_bw()
  g6 <- ggplot(plotdf, aes_string(xvars[2], ".resid")) + geom_point() +
    geom_smooth(se = FALSE) + scale_x_continuous(xvars[2]) +
    scale_y_continuous("Residuals") +
    labs(title = paste0("Residuals Against ", xvars[2])) + theme_bw()
  grid.arrange(g1, g2, g3, g4, g5, g6, ncol=2)
}
