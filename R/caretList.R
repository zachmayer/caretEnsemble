#' Extract the tuning parameters from a list provided by the user
#'
#' @param x a list of tuning parameters and methods passed by the user
#' @return A simple list of the tuning parameters
tuneExtract <- function(x){
  tuneType <- paste(names(x))
  tunePar <- x[[1]]
  return(list(tuneType = tuneType, tunePar = tunePar))
}

#' Create uniform seeds across model fits to allow for ensembling
#'
#'
#' @param ctrl a \code{\link{trainControl}} object passed by the user
#' @param M the maximum number of resamples necessary
#' @return A \code{\link{trainControl}} object with a new slot, seeds
#' @details Currently the seed structure is determined with the length of the
#' seed list being number * repeats +1 and the length of all vectors B -1 being
#' 20 * tuneLength^2 with the final vector being a single seed
setSeeds <- function(ctrl, M){
  # M is the square of the tune-length
  B <- ctrl$number * ctrl$repeats
  mseeds <- vector(mode = "list", length = B + 1)
  if(length(M) > 1){M <- max(M)}
  M <- M * 20 # hack for arbitrary scalar
  for(i in 1:B) mseeds[[i]] <- .Random.seed[1:M] # is this the best way?
  mseeds[[B+1]] <- .Random.seed[1]
  ctrl$seeds <- mseeds
  return(ctrl)
}

#' Helper function to extract tune grids from arguments passed to tuneList
#'
#' Returns the tuneGrids of the tuneList
#'
#' @param x list of tuning parameters for train objects
#' @return The maximum length of any tuneGrid in the list
tmpExtract <- function(x){
  if(is.null(names(x))){
    tmp <- 0
  } else if(names(x) == "tuneGrid"){
    tmp <- nrow(x$tuneGrid)
  } else{
    tmp <- max(x$tuneLength)
  }
  #tmp <- ifelse(length(y) > 1, length(y), max(y))
  return(tmp)
}

#' Create a list of several train models from the caret package
#'
#' Build a list of train objects suitable for ensembling using the \code{\link{caretEnsemble}}
#' function.
#'
#' @param methodList a character vector of caret models to ensemble.
#' @param control a \code{\link{trainControl}} object
#' @param x a matrix of predictors
#' @param y the dependent variable being predicted
#' @param tuneList optional, a list of the length of \code{methodList} with the tuning parameters for each method
#' @param baseSeed optional, to preserve reproducibility, a base seed to be used in the resampling randomization
#' @param tuneLength optional, the length of tuning to be done
#' @param ... additional arguments to pass to \code{\link{train}}
#' @export
#' @return A list of \code{\link{train}} objects
buildModels <- function(methodList, control, x, y, tuneList = NULL, baseSeed = NULL, tuneLength = NULL, ...) {
  if(!missing(tuneList)){
    methodList <- names(tuneList)
    suppressWarnings(tmp <- lapply(tuneList, tmpExtract))
    tmp2 <- lapply(methodList, function(x) length(unique(modelLookup(x)$parameter)))
    tmp <- as.numeric(tmp)
    tmp2 <- as.numeric(tmp2)
    M <- max(tmp^tmp2)
  } else if(missing(tuneList)){
    tl <- ifelse(missing(tuneLength), 6, tuneLength)
    M <- tl ^ 2
  }
  seedFlag <- length(control$seeds)
  if(seedFlag < 2){ # if custom seeds are already present in trainControl, keep them
    if(missing(tuneList)){ctrl <- setSeeds(control, M = M)}
    else {
      ctrl <- setSeeds(control, M = M)
    }
  }
  #
  if(!missing(tuneList)){methodList <- names(tuneList)}
  if(missing(baseSeed)){baseSeed <- .Random.seed[1]}
  #
  modelList <- sapply(methodList, function(x) NULL)

  for(i in methodList){
    set.seed(baseSeed) # so splits are same across resamples
    if(missing(tuneList)){
      modelList[[i]] <- train(x = x, y=y, method = i, trControl = ctrl,
                              tuneLength = tl, ...)
    } else {
      tmpTune <- tuneExtract(tuneList[[i]])
      if(length(tmpTune$tuneType)==0){
        tmpTune$tuneType <- "tuneLength"
      }
      if(is.null(tmpTune$tunePar)){
        warning("No tuning parameters provided, using default of tuneLength = 6")
        tmpTune$tunePar <- 6
      }
      modelList[[i]] <- eval(parse(text = paste("train(x = x, y=y, method = i,
                                                trControl = ctrl,", tmpTune$tuneType,
                                                " = tmpTune$tunePar, ...)")))
      tmpTune <- NULL; Tune <- NULL
    }

  }
  return(modelList)
}
