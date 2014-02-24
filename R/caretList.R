set.seed(442)
library(caret)
train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3, 
                     noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3, 
                    noiseVars = 10, corrVars = 4, corrValue = 0.6)





ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

#control <- trainControl(...)
myControl = trainControl(method = "cv", number = 3, repeats = 1, 
                         p = 0.75, savePrediction = TRUE, 
                         classProbs = TRUE, returnResamp = "final", 
                         returnData = TRUE)

methodList <- list(rf=list(tuneGrid=data.frame(.mtry=c(2,4,8,1)), 
                           nnet=list(tuneLength=10)), 
                   knn=list(tuneLength=25))

setSeeds <- function(ctrl, M){
  B <- ctrl$number * ctrl$repeats
  mseeds <- vector(mode = "list", length = B + 1)
  for(i in 1:B) mseeds[[i]] <- sample.int(1000, M + 1)
  mseeds[[B+1]] <- sample.int(1000, 1)
  ctrl$seeds <- mseeds
  return(ctrl)
}

mylist <- sapply(methodList,function(x) NULL)

buildModels <- function(methodList, control, x, y, tuneList = NULL, ...) {
  if(missing(tuneList)){
    tl <- 6
  }
  M <- tl ^ 2
  ctrl <- setSeeds(control, M = M)
  modelList <- sapply(methodList,function(x) NULL)
  baseSeed <- .Random.seed[1]
  for(i in methodList){
    set.seed(baseSeed)
    modelList[[i]] <- train(x = x, y=y, method = i, trControl = ctrl, ...)
  }
  return(modelList)
}

test1 <- buildModels(methodList = c("knn", "glm"), control = myControl, 
                     x = train[, -23], 
                    y = train[, "Class"])


test2 <- buildModels(methodList = c("knn", "glm", "treebag", "nnet"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"])

ens1 <- caretEnsemble(test1)
summary(ens1)

ens2 <- caretEnsemble(test2)
summary(ens2)

fullModel <- train(Class ~ ., data = train, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)