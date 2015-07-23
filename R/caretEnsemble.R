#' @title Combine several predictive models via weights
#'
#' @description Find a good linear combination of several classification or regression models,
#' using either linear regression, elastic net regression, or greedy optimization.
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
#' @param optFUN the optimization function to use
#' @param ... additional arguments to pass to the optimization function
#' @return a \code{\link{caretEnsemble}} object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
#' @export
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50,1:2], iris[1:50,3], methodList=c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' summary(ens)
#' }
caretEnsemble <- function(all.models, optFUN=NULL, ...){

  stopifnot(is(all.models, "caretList"))

  #Check the models, and make a matrix of obs and preds
  predobs <- makePredObsMatrix(all.models)

  # Check that missingness is consistent across models in library
  if(anyNA(predobs$preds)){
    warning("Missing values found in predictions. Check library models.")
    nacheck <-apply(predobs$preds, 2, function(x) length(which(is.na(x))))
    if(abs(max(nacheck) - min(nacheck)) > 0.01){
      warning("Missingness is not consistent across models. Final model weights may be biased.")
    }
  }

  #If the optimization function is NULL, choose default
  if (is.null(optFUN)){
    if (predobs$type=="Classification") {
      optFUN <- greedOptAUC
    } else {
      optFUN <- greedOptRMSE
    }
  }

  #Determine weights
  weights <- optFUN(predobs$preds, predobs$obs, ...)
  weights[! is.finite(weights)] <- 0

  #Normalize and name weights
  weights <- weights/sum(weights)
  names(weights) <- sapply(all.models, function(x) x$method)

  #Remove 0-weighted models
  keep <- which(weights != 0)

  # Make sure NAs in 0 weighted models do not cause problems
  if(anyNA(predobs$preds)){
    if(length(keep) == 1){
      weightedPreds <- predobs$preds[, keep] * weights[keep]
    } else if(length(keep > 1)){
      weightedPreds <- predobs$preds[, keep] %*% weights[keep]
    }
    if (predobs$type == "Regression"){
      error <- RMSE(weightedPreds, predobs$obs, na.rm=TRUE)
      names(error) <- "RMSE"
    } else {
      metric <- "AUC"
      error <- caTools::colAUC(weightedPreds, predobs$obs)
      names(error) <- "AUC"
    }
  } else{
    if (predobs$type == "Regression"){
      error <- RMSE(predobs$preds %*% weights, predobs$obs, na.rm=TRUE)
      names(error) <- "RMSE"
    } else {
      metric <- "AUC"
      error <- caTools::colAUC(predobs$preds %*% weights, predobs$obs)
      names(error) <- "AUC"
    }
  }

  #Return final model
  models <- all.models[keep]
  class(models) <- "caretList"
  out <- list(models=models, weights=weights[keep], error=error,
              modelType = predobs$type)
  class(out) <- "caretEnsemble"
  return(out)
}

#' Make predictions from a caretEnsemble. This function passes the data to each function in
#' turn to make a matrix of predictions, and then multiplies that matrix by the vector of
#' weights to get a single, combined vector of predictions.
#' @param object a \code{\link{caretEnsemble}} to make predictions from.
#' @param keepNA a logical indicating whether predictions should be made for all
#' cases where sufficient data exists or only for complete cases across all models. When
#' TRUE this does not predict for missing values. When FALSE, missing values are overwritten
#' with predictions where possible.
#' @param se logical, should prediction errors be produced? Default is false.
#' @param return_weights a logical indicating whether prediction weights for each model for
#' each observation should be returend
#' @param ... arguments (including newdata) to pass to predict.train. These arguments
#' must be named
#' @return If \code{return_weights = TRUE} a list is returned with a data.frame
#' slot for predictions and a matrix slot for the model weights. If \code{return_weights = FALSE}
#' a data.frame is returned for the predictions.
#' @export
#' @method predict caretEnsemble
#' @examples
#' \dontrun{
#' set.seed(42)
#' models <- caretList(iris[1:50,1:2], iris[1:50,3], methodList=c("glm", "lm"))
#' ens <- caretEnsemble(models)
#' cor(predict(ens, newdata=iris[51:150,1:2]), iris[51:150,3])
#' }
predict.caretEnsemble <- function(object, keepNA = TRUE, se = FALSE, return_weights = FALSE, ...){
  stopifnot(is(object$models, "caretList"))
  # Default se to FALSE
  if(!return_weights %in% c(TRUE, FALSE)){
    return_weights <- FALSE
    warning("return_weights not set properly, default set to FALSE")
  }
  modtype <- extractModelTypes(object$models)
  preds <- predict(object$models,  ...)
  if(keepNA == TRUE){
    if(anyNA(preds)){
      message("Predictions being made only for cases with complete data")
    }
    est <- as.numeric(preds %*% object$weights)
    if(dim(preds)[1]<1000){ # use apply for small data
      se.tmp <- apply(preds, 1, FUN = wtd.sd, weights = object$weights, normwt = TRUE)
    } else { # switch to foreach for large data
      require('foreach')
      se.tmp <- foreach(n=1:dim(preds)[1], .combine= c) %dopar% {
        pred <- preds[n, ]
        wtd.sd(pred, weights = object$weights, normwt = TRUE)
      }
    }
  } else if(keepNA == FALSE){
    if(anyNA(preds)){
    message("Predictions being made only from models with available data")
    }
    conf <- ifelse(is.na(preds), NA, 1)
    conf <- sweep(conf, MARGIN=2, object$weights,`*`)
    conf <- apply(conf, 1, function(x) x / sum(x, na.rm=TRUE))
    conf <- t(conf); conf[is.na(conf)] <- 0

    if(dim(preds)[1]<1000){ # use apply for small data
      est <- apply(preds, 1, function(x){
        weighted.mean(x, w=object$weights, na.rm = TRUE)
      })
      se.tmp <- apply(preds, 1, FUN = wtd.sd, weights = object$weights,
                      normwt = TRUE, na.rm = TRUE)
    } else { # switch to foreach for large data
      require('foreach')
      est <- foreach(n=1:dim(preds)[1], .combine= c) %dopar% {
        pred <- preds[n, ]
        weighted.mean(pred, w=object$weights, na.rm = TRUE)
      }
      se.tmp <- foreach(n=1:dim(preds)[1], .combine= c) %dopar% {
        pred <- preds[n, ]
        wtd.sd(pred, weights = object$weights, normwt = TRUE, na.rm = TRUE)
      }
    }
  }
  if(return_weights == FALSE){
    if(se == FALSE){
      return(est)
    } else {
      return(data.frame(pred = est, se = se.tmp))
    }
  } else if(return_weights == TRUE) {
    if(se == FALSE){
      if(keepNA == FALSE){
        out <- list(preds = est, weight = conf)
        return(out)
      } else if(keepNA == TRUE){
        wghtMat <- matrix(object$weights, c(1, length(object$weights)),
                          dimnames = list(NULL, names(object$weights)))
        return(list(preds = est,
                    weight = wghtMat))
      }
    } else if (se == TRUE){
      if(keepNA == FALSE){
        return(list(preds = data.frame(pred = est, se = se.tmp), weight = conf))
      }
      wghtMat <- matrix(object$weights, c(1, length(object$weights)),
                        dimnames = list(NULL, names(object$weights)))
      return(list(preds = data.frame(pred = est, se = se.tmp),
                  weight = wghtMat))
    }
  }
}

#' @title Summarize the results of caretEnsemble for the user.
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
  if(is.null(types)){
    types <- as.vector(strsplit(paste0("model", 1:length(object$models)), split = " ",
                                fixed = TRUE), mode = "character")
  }
  types <- paste(types, collapse = ", ")
  wghts <- object$weights
  metric <- names(object$error)
  val <- object$error[[1]]
  cat(paste0("The following models were ensembled: ", types, " \n"))
  cat("They were weighted: \n")
  cat(paste0(paste0(wghts, collapse = " "), "\n"))
  cat(paste0("The resulting ", metric, " is: ", round(val, 4), "\n"))

  # Add code to compare ensemble to individual models
  cat(paste0("The fit for each individual model on the ", metric, " is: \n"))
  print(extractModRes(object), row.names = FALSE)
}

#' Extract the model accuracy metrics of the individual models in an ensemble object.
#' @param ensemble a caretEnsemble to make predictions from.
extractModRes <- function(ensemble){
  if(class(ensemble) != "caretEnsemble") stop("extractModRes requires a caretEnsemble object")
  methods <- names(ensemble$models)
  if(is.null(methods)){
#     methods <- as.vector(strsplit(paste0("model", 1:length(ensemble$models)), split = " ",
#                                 fixed = TRUE), mode = "character")
    methods <- unlist(lapply(ensemble$models, "[[", 1))
  } #sanitize names
  metric <- names(ensemble$error)
  modRes <- data.frame(method = methods,
                       metric = unlist(lapply(ensemble$models,
                                              getMetric.train,
                                              metric = metric)),
                       metricSD = unlist(lapply(ensemble$models,
                                                getMetricSD.train,
                                                metric = metric)),
                       stringsAsFactors = FALSE) # prefill data frame
  return(modRes)
}

#' Extract accuracy metrics from a model
#' @rdname metrics
#' @export
getMetric <- function(x, ...){
  UseMethod("getMetric")
}

#' Extract a model accuracy metric from a \code{\link{train}} object.
#' @param x a caretEnsemble object
#' @param metric Which metric to extract
#' @param ... Passed between metric functions
#' @return A numeric representing the metric desired metric.
#' @rdname metrics
#' @export
getMetric.train <- function(x, metric= c("AUC", "RMSE"), ...){
  if(missing(metric)){
    metric <- ifelse(x$modelType == "Regression", "RMSE", "AUC")
    warning("Metric not specified, so default is being chosen.")
  }
  metricTest <- ifelse(metric == "AUC", "Classification", "Regression")
  stopifnot(x$modelType == metricTest)
  if(metric == "AUC"){
    return(getAUC(x))
  } else if(metric == "RMSE"){
    return(getRMSE(x))
  }
}

#' Extract the AUC metric from a \code{\link{train}} object.
#' @return A numeric for the AUC of the best model
#' @rdname metrics
#' @note AUC extracted from a train object is for all resamples pooled, not the average
#' of the AUC for each resample.
getAUC <- function(x){
  UseMethod("getAUC")
}

#' @importFrom caTools colAUC
getAUC.train <- function(x){
  if(x$modelType != "Classification"){
    stop("AUC can only be calculated for classification models")
  }
  bestPerf <- x$bestTune
  colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
  dat <- merge(x$pred, bestPerf)
  z <- table(dat$obs)
  prevOutcome <- names(z)[z == max(z)]
  AUC <- colAUC(dat[, prevOutcome], dat$obs)
  return(as.numeric(AUC))
}

#' Extract the RMSE metric from a model object.
#' @return A numeric for the RMSE of the best model
#' @rdname metrics
#' @note RMSE extracted from a train object is for all resamples pooled, not the average
#' of the RMSE for each resample. All missing values are ignored.
getRMSE <- function(x){
  UseMethod("getRMSE")
}

getRMSE.train <- function(x){
  #TODO: decide about NAs
  if(x$modelType != "Regression"){
    stop("RMSE can only be calculated for regression models")
  }
  bestPerf <- x$bestTune
  colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
  dat <- merge(x$pred, bestPerf)
  z <- table(dat$obs)
  prevOutcome <- names(z)[z == max(z)]
  out <- RMSE(dat$pred, dat$obs, na.rm=TRUE)
  return(as.numeric(out))
}

#' Extract the standard deviation from resamples for an accuracy metric from
#' a model object.
#' @param x an object with model performanc metrics
#' @param metric a character, either "RMSE" or "AUC" indicating which metric to extract
#' @param which a character, either "all" or "best", default is best, see details
#' @return A numeric for the standard deviation of the selected metric across
#' tuning parameters and resamples in the original object.
#' @details Which allows the user to select whether to generate a standard deviation
#' for the performance metric across all values of the tuning parameters and resamples,
#' or only for resamples under the best tuning parameter. Missing values are ignored.
#' @rdname metricsSD
getMetricSD <- function(x, metric, which = c("all", "best")){
  UseMethod("getMetricSD")
}

getMetricSD.train <- function(x, metric = c("RMSE", "AUC"), which = c("all", "best")){
  if(missing(metric)){
    metric <- ifelse(x$modelType == "Regression", "RMSE", "AUC")
    message(paste0("Metric not specified, so default is being chosen: ", metric))
  }
  if(missing(which)){
    which <- "best"
#     message("which not specified so sd only calculated for best values of tuning parameters")
  }
  metricTest <- ifelse(metric == "AUC", "Classification", "Regression")
  stopifnot(x$modelType == metricTest)
  if(metric == "AUC"){
    z <- table(x$pred$obs)
    prevOutcome <- names(z)[z == max(z)]
    out <- by(x$pred[, c(prevOutcome, "obs")], x$pred[, c(names(x$bestTune), "Resample")],
       function(x) colAUC(x[,1], x[,2]))
  } else if(metric == "RMSE"){
    out <- by(x$pred[, c("pred","obs")], x$pred[, c(names(x$bestTune), "Resample")],
              function(x) RMSE(x[,1], x[,2], na.rm=TRUE))
  }
  if(which == "best"){
    out <- matchBestTune(out, x$bestTune)
    out <- sd(as.numeric(out))
  } else{
    out <- sd(as.numeric(out))
  }
  return(out)
}

#' @keywords internal
matchBestTune <- function(out, bt){
  nams <- names(attributes(out)$dimnames)
  nams <- nams[nams %in% names(bt)]
  tmp <- c()
  for(i in length(nams)){
    tmp.t <- out[attributes(out)$dimnames[[nams[[i]]]] == as.character(bt[,nams[i]])]
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
#' @param scale should importance values be scaled 0 to 100?
#' @param weight should a model weighted importance be returned?
#' @param ... other arguments to be passed to varImp
#' @return A \code{\link{data.frame}} with one row per variable and one column
#' per model in object
#' @importFrom digest digest
#' @export
varImp.caretEnsemble <- function(object, scale = TRUE, weight = TRUE, ...){
  a <- lapply(object$models, varImp)
  # grab method names
  names(a) <- make.unique(unlist(lapply(object$models, "[[", 1)), sep = "_")
  # drop duplicates
  a <- a[!duplicated(lapply(a, digest::digest))]
  # add a check to drop multiple columns
  a <- lapply(a, clean_varImp)
  dat <- varImpFrame(a)
  if(scale == TRUE){
    dat[,-1] <- apply(dat[, -1], 2, function(d) d / sum(d, na.rm = TRUE) * 100)
  }
  if(weight == FALSE){
    names(dat) <- c("variable", names(a))
    return(dat)
  } else{
    wghts <- object$weights[names(object$weights) %in% names(a)]
    #weight
    wght <- apply(dat[, -1], 1, function(d) weighted.mean(d, w = wghts, na.rm=TRUE))
    if(scale == TRUE){
      wght <- (wght / sum(wght, na.rm=TRUE)) * 100
    }
    dat <- cbind(dat, wght)
    names(dat) <- c("variable", names(a), "ensemble")
    dat <- as.data.frame(dat)
    if(scale == FALSE){
      warning("Weighting of unscaled importance factors may not make sense. Try again with scale = TRUE.")
      return(dat)
    } else{
      return(dat)
    }
  }
}

# Break into get varImp
# weight varImp
clean_varImp <- function(x){
  names(x$importance)[1] <- "Overall"
  x$importance <- x$importance[,"Overall", drop=FALSE]
  return(x$importance)
}

varImpFrame <- function(x){
  dat <- do.call(rbind.data.frame, x)
  # data.frame
  dat <- dat[!duplicated(lapply(dat, summary))]
  # Parse frame
  dat$id <- row.names(dat)
  dat$model <- sub("\\.[^\n]*", "",dat$id)
  dat$var <- sub("^[^.]*", "", dat$id)
  dat$var <- substr(dat$var, 2, nchar(dat$var))
  # Parse intercept variables
  dat$var[grep("Inter", dat$var)] <- "Intercept"
  dat$id <- NULL
  row.names(dat) <- NULL
  dat <- reshape(dat, direction = "wide", v.names="Overall",
                 idvar = "var", timevar = "model")
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
    yhat <- predict(object)
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
fortify.caretEnsemble <- function(model, data = NULL, ...){
  data <- extractModFrame(model)
  data$y <- model$models[[1]]$trainingData$.outcome
  if(class(data$y) != "numeric"){
    data$y <- as.character(data$y)
    z <- table(data$y)
    prevOutcome <- names(z)[z == max(z)]
    data$y <- ifelse(data$y == prevOutcome, 0, 1)
    data$y <- as.numeric(data$y)
  }
  data$.fitted <- predict(model)
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
#' @import ggplot2
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
  dat <- extractModRes(x)
  metricLab <- names(x$error)
  ggplot(dat, aes(x = method, y = metric, ymin = metric - metricSD,
                  ymax = metric + metricSD)) + geom_pointrange() +
    geom_hline(yintercept = x$error, color = I("red"), size = I(1.1)) +
    theme_bw() + labs(x = "Individual Model Method", y = metricLab)
}

#' @title Convenience function for more in-depth diagnostic plots of caretEnsemble objects
#' @description This function provides a more robust series of diagnostic plots
#' for a caretEnsemble object.
#' @param object a \code{caretEnsemble} object
#' @param which an integer index for which of the plots to print
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
#' @import ggplot2
#' @import grid
#' @import plyr
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
autoplot.caretEnsemble <- function(object, which = c(1:6), mfrow = c(3, 2),
                                   xvars = NULL, ...){
  plotdf <- suppressMessages(fortify(object))
  g1 <- plot(object) + labs(title = "Metric and SD For Component Models")
  wghtFrame <- as.data.frame(object$weights)
  wghtFrame$method <- row.names(wghtFrame)
  names(wghtFrame) <- c("weights", "method")
  g2 <- ggplot(plotdf, aes(.fitted, .resid)) + geom_point() + geom_smooth(se = FALSE) +
    geom_hline(linetype = 2, size = 0.2) + scale_x_continuous("Fitted Values") +
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
  #   g4 <- ggplot(plotdf, aes_string(xvars[1], ".resid")) + geom_point() +
  #     geom_smooth(se = FALSE) + scale_x_continuous(xvars[1]) +
  #     scale_y_continuous("Residuals") +
  #     labs(title = paste0("Residuals Against ", xvars[1])) + theme_bw()
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
