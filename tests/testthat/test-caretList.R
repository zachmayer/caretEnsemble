# Test caretList

set.seed(442)
library(caret)
library(randomForest)
train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3, 
                     noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3, 
                    noiseVars = 10, corrVars = 4, corrValue = 0.6)

#######################
# Classification models
########################

# Specify controls
myControl = trainControl(method = "cv", number = 3, repeats = 1, 
                         p = 0.75, savePrediction = TRUE, 
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE, returnResamp = "final", 
                         returnData = TRUE, verboseIter = FALSE)


# Simple two method list
test1 <- buildModels(methodList = c("knn", "glm"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"])


# Simple 4 method list
test2 <- buildModels(methodList = c("knn", "glm", "treebag"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], metric = "ROC")

ens2 <- caretEnsemble(test2)
summary(ens2)

test3 <- buildModels(methodList = c("pda", "lda2", "multinom", "bagFDA", "nnet", "gbm"), 
                     control = myControl, 
                     x = train[, -23], 
                     y = train[ , "Class"], metric = "ROC")

ens3 <- caretEnsemble(test3)
summary(ens3)

mypreds <- predict(ens3)

mypreds <- predict(ens3, keepNA = TRUE, newdata = train[1:50, -23])

"pls"      "lda2"     "multinom" "bagFDA"   "nnet"     "gbm"     


predict2.caretEnsemble <- function(object, keepNA = TRUE, se = TRUE, ...){
  type <- checkModels_extractTypes(object$models)
  preds <- multiPredict(object$models, type, ...)
  if(keepNA == TRUE){
    message("Predictions being made only for cases with complete data")
    out <- as.numeric(preds %*% object$weights)
  } else {
    message("Predictions being made only from models with available data")
    conf <- ifelse(is.na(preds), NA, 1)
    conf <- sweep(conf, MARGIN=2, object$weights,`*`)
    conf <- apply(conf, 1, function(x) x / sum(x, na.rm=TRUE))
    conf <- t(conf); conf[is.na(conf)] <- 0
    point <- apply(point, 1, function(x){weighted.mean(x, w=object$weights, na.rm=TRUE)})
    out <- list(predicted = point, weight = conf)
  }
  if(se = FALSE){
   return(out) 
  } else{
    se <- 
  }
  return(out)
}



# calculate SE

## Weighted sd
## From Hmisc wtd.var
wtd.sd <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) {
  if (!length(weights)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    return(sd(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt) 
    weights <- weights * length(x)/sum(weights)
  xbar <- sum(weights * x)/sum(weights)
  out <- sqrt(sum(weights * ((x - xbar)^2))/(sum(weights) - 1))
  return(out)
}


test2 <- buildModels(methodList = c("knn", "glm", "treebag", "nnet"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], tuneLength = 4, baseSeed = 3252)


# User specified tuneLength 
# Simple two method list
test1 <- buildModels(methodList = c("knn", "glm"), control = myControl, 
                     x = train[, -23], tuneLength = 9, 
                     y = train[, "Class"])


# Simple 4 method list
test2 <- buildModels(methodList = c("knn", "nnet", "treebag"), control = myControl, 
                     x = train[, -23], tuneLength = 15, 
                     y = train[, "Class"], metric = "ROC")


# User specified metric



# User specifies methods and tuning parameters specifically using a tuneList
tuneTest <- list(rf=list(tuneGrid=data.frame(.mtry=c(2,4,8,1))), 
                 nnet=list(tuneLength=10), 
                 knn=list(tuneLength=25))

# Simple with mix of data.frame and tuneLength

test2a <- buildModels(tuneList = tuneTest, control = myControl,  x = train[, -23], 
                     y = train[, "Class"])

# More complex with multidimensional tuneGrid and NULL tuneLengths
tuneTest2 <- list(glm = list(NULL), nnet = list(tuneLength = 10), 
                  treebag = list(tuneLength = 2), 
                  avNNet = list(tuneGrid = data.frame(.size = c(1, 3, 5), 
                                                      .decay = c(0.8, 0.5, 0.2), 
                                                      .bag = c(1, 20, 40))))


test3a <- buildModels(tuneList = tuneTest2, control = myControl,  x = train[, -23], 
                      y = train[, "Class"])

# Throws warning, but we're good

# Regression
myControl2 = trainControl(method = "cv", number = 3, repeats = 1, 
                          p = 0.75, savePrediction = TRUE, 
                          returnResamp = "final", 
                          returnData = TRUE, verboseIter = FALSE)

test3 <- buildModels(methodList = c("glm", "lm"), control = myControl2, 
                     x = train[, c(-23, -1)], 
                     y = train[, 1])


test4 <- buildModels(methodList = c("glm", "treebag", "nnet", "lm"), control = myControl2, 
                     x = train[, c(-23, -1)], 
                     y = train[, 1])



ens1 <- caretEnsemble(test1)
summary(ens1)

ens2 <- caretEnsemble(test2)
summary(ens2)

ens3 <- caretEnsemble(test3)
summary(ens3)

ens4 <- caretEnsemble(test4)
summary(ens4)

ens2a <- caretEnsemble(test2a)
summary(ens2a)

ens3a <- caretEnsemble(test3a)
summary(ens3a)

# predictions

mypreds <- predict(ens3a, keepNA = TRUE, newdata = test)
length(mypreds)
mypreds <- predict(ens3a, keepNA = FALSE, newdata = test)
length(mypreds)


# Test baseSeed is preserved if specified