## Test optimizers


set.seed(442)
library(caret)


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
                     y = train[, "Class"], metric = "ROC")


# Simple 4 method list
test2 <- buildModels(methodList = c("knn", "glm", "treebag"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], metric = "ROC")


context("Test optimization procedure for AUC")

predobs <- makePredObsMatrix(test2)

weights1 <- qpOptAUC(predobs$preds, as.integer(predobs$obs))
weights2 <- qpOptAUC(predobs$preds, as.integer(predobs$obs), offset=TRUE)
weights3 <- greedOptAUC(predobs$preds, predobs$obs)

weights[! is.finite(weights)] <- 0

ens <- caretEnsemble(test2, optFUN=qpOptAUC, offset = TRUE)
ens2 <- caretEnsemble(test2, optFUN=qpOptAUC)
ens.old <- caretEnsemble(test2)


#Normalize and name weights
weights3 <- weights3/sum(weights3)
names(weights) <- sapply(all.models, function(x) x$method)

#Remove 0-weighted models
keep <- which(weights != 0)

# Regression
myControl2 = trainControl(method = "cv", number = 3, repeats = 1, 
                          p = 0.75, savePrediction = TRUE, 
                          returnResamp = "final", 
                          returnData = TRUE, verboseIter = FALSE)

test1 <- buildModels(methodList = c("glm", "lm", "rlm", "pls"), control = myControl2, 
                     x = train[1:100, c(-23, -1)], 
                     y = train[1:100, 1])


test2 <- buildModels(methodList = c("glm", "ppr", "lm"), control = myControl2, 
                     x = train[1:100, c(-23, -1)], 
                     y = train[1:100, 1])



predobs <- makePredObsMatrix(test1)
weights1 <- greedOptRMSE(predobs$preds, predobs$obs)
weights2 <- qpOptRMSE(predobs$preds, predobs$obs)



ens1 <- caretEnsemble(test1)

ens2 <- caretEnsemble(test2)


