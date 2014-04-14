

tuneList <- list(rf=list(tuneGrid=data.frame(.mtry=c(2,4,8,1))), 
                           nnet=list(tuneLength=10), 
                   knn=list(tuneLength=25))

tuneExtract <- function(x){
  tuneType <- paste(names(x))
  tunePar <- x[[1]]
  return(list(tuneType = tuneType, tunePar = tunePar))
}

setSeeds <- function(ctrl, M){
  # M is the square of the tune-length
  B <- ctrl$number * ctrl$repeats
  mseeds <- vector(mode = "list", length = B + 1)
  if(length(M) > 1){M <- max(M)}
  for(i in 1:B) mseeds[[i]] <- .Random.seed[1:M] # is this the best way?
  mseeds[[B+1]] <- .Random.seed[1]
  ctrl$seeds <- mseeds
  return(ctrl)
}

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

# TODO: allow user to pass pre-specified seeds or extract random seeds
# TODO: consider making a tuneList method to identify the seed structure and build seeds

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
    set.seed(baseSeed)
    if(missing(tuneList)){
      modelList[[i]] <- train(x = x, y=y, method = i, trControl = ctrl, ...)
    } else {
      #stop("Not yet written")
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
                                                " = tmpTune$tunePar)")))
      tmpTune <- NULL; Tune <- NULL
    }
   
  }
  return(modelList)
}
