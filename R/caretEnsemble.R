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
#'
#' @param all.models a list of caret models to ensemble.
#' @param optFUN the optimization function to use
#' @param ... additional arguments to pass to the optimization function
#' @return a \code{\link{caretEnsemble}} object
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
#' @export
caretEnsemble <- function(all.models, optFUN=NULL, ...){
  #Check the models, and make a matrix of obs and preds
  predobs <- makePredObsMatrix(all.models)

  #If the optimization function is NULL, choose default
  if (is.null(optFUN)){
    if (predobs$type=='Classification') {
      optFUN <- greedOptAUC
    } else { optFUN <- greedOptRMSE }
  }

  #Determine weights
  weights <- optFUN(predobs$preds, predobs$obs, ...)
  weights[! is.finite(weights)] <- 0

  #Normalize and name weights
  weights <- weights/sum(weights)
  names(weights) <- sapply(all.models, function(x) x$method)

  #Remove 0-weighted models
  keep <- which(weights != 0)

  #Determine RMSE
  if (predobs$type == "Regression"){
    error <- RMSE(predobs$preds %*% weights, predobs$obs)
    names(error) <- 'RMSE'
  } else {
    metric <- 'AUC'
    error <- caTools::colAUC(predobs$preds %*% weights, predobs$obs)
    names(error) <- 'AUC'
  }

  #Return final model
  out <- list(models=all.models[keep], weights=weights[keep], error=error)
  class(out) <- 'caretEnsemble'
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
#' @param ... arguments (including newdata) to pass to predict.train. These arguments
#' must be named
#' @method predict caretEnsemble
#' @export
predict.caretEnsemble <- function(object, keepNA = TRUE, se = NULL, ...){
  # Default se to FALSE
  if(missing(se)){se <- FALSE}
  modtype <- checkModels_extractTypes(object$models)
  preds <- multiPredict(object$models, type = modtype, ...)
  if(keepNA == TRUE){
    message("Predictions being made only for cases with complete data")
    out <- as.numeric(preds %*% object$weights)
    se.tmp <- apply(preds, 1, FUN = wtd.sd, weights = object$weights, normwt = TRUE)
  } else if(keepNA == FALSE){
    message("Predictions being made only from models with available data")
    conf <- ifelse(is.na(preds), NA, 1)
    conf <- sweep(conf, MARGIN=2, object$weights,`*`)
    conf <- apply(conf, 1, function(x) x / sum(x, na.rm=TRUE))
    conf <- t(conf); conf[is.na(conf)] <- 0
    est <- apply(preds, 1, function(x){weighted.mean(x, w=object$weights, na.rm = TRUE)})
    se.tmp <- apply(preds, 1, FUN = wtd.sd, weights = object$weights, normwt = TRUE, na.rm = TRUE)
    out <- list(predicted = est, weight = conf)
  }
  if(se == FALSE){
    return(out)
  } else{
    if(keepNA == FALSE){
      return(list(preds = data.frame(pred = est, se = se.tmp), weight = conf))
    }
    return(data.frame(pred = out, se = se.tmp))
  }
}

#' @title Summarize the results of caretEnsemble for the user.
#' @param object a \code{\link{caretEnsemble}} to make predictions from.
#' @param ... optional additional parameters.
#' @export
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
#' @export
extractModRes <- function(ensemble){
  if(class(ensemble) != "caretEnsemble") stop("extractModRes requires a caretEnsemble object")
  methods <- names(ensemble$models)
  if(is.null(methods)){
    methods <- as.vector(strsplit(paste0("model", 1:length(ensemble$models)), split = " ",
                                fixed = TRUE), mode = "character")
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

#' Extract a model accuracy metric from an S3 object.
#' @param x an object with model performanc metrics
#' @param metric a character, either "RMSE" or "AUC" indicating which metric to extract
#' @return A numeric representing the metric desired metric.
#' @rdname metrics
#' @export
getMetric <- function(x, metric){
  UseMethod("getMetric")
}

#' Extract a model accuracy metric from a \code{\link{train}} object.
#' @rdname metrics
#' @export
getMetric.train <- function(x, metric= c("AUC", "RMSE")){
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
#' @export
getAUC <- function(x){
  UseMethod("getAUC")
}

#' @export
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
#' @export
getRMSE <- function(x){
  UseMethod("getRMSE")
}

#' @export
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
  out <- RMSE(dat$pred, dat$obs)
  return(as.numeric(out))
}


#' Extract the standard deviation from resamples for an accuracy metric from
#' a model object.
#' @param x an object with model performanc metrics
#' @param metric a character, either "RMSE" or "AUC" indicating which metric to extract
#' @return A numeric for the standard deviation of the selected metric across
#' tuning parameters and resamples in the original object.
#' @rdname metricsSD
#' @export
getMetricSD <- function(x, metric){
  UseMethod("getMetricSD")
}

#' @export
getMetricSD.train <- function(x, metric = c("RMSE", "AUC")){
  if(missing(metric)){
    metric <- ifelse(x$modelType == "Regression", "RMSE", "AUC")
    warning("Metric not specified, so default is being chosen.")
  }
  metricTest <- ifelse(metric == "AUC", "Classification", "Regression")
  stopifnot(x$modelType == metricTest)
  if(metric == "AUC"){
    z <- table(x$pred$obs)
    prevOutcome <- names(z)[z == max(z)]
    out <- by(x$pred[, c(prevOutcome, "obs")], x$pred[, "Resample"],
       function(x) colAUC(x[,1], x[,2]))
  } else if(metric == "RMSE"){
    out <- by(x$pred[, c("pred","obs")], x$pred[, "Resample"],
              function(x) RMSE(x[,1], x[,2]))
  }
  out <- sd(as.numeric(out))
  return(out)
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
#' @importFrom caret varImp
#' @method varImp caretEnsemble
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

do.call(library, list('methods', 'caret'))
