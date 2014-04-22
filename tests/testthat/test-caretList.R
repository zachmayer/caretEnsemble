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
                         classProbs = TRUE, returnResamp = "final", 
                         returnData = TRUE, verboseIter = FALSE)


# Simple two method list
test1 <- buildModels(methodList = c("knn", "glm"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"])


# Simple 4 method list
test2 <- buildModels(methodList = c("knn", "glm", "treebag", "nnet"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"])

ens2 <- caretEnsemble(test2)
summary(ens2)


test2 <- buildModels(methodList = c("knn", "glm", "treebag", "nnet"), control = myControl, 
                     x = train[, -23], 
                     y = train[, "Class"], tuneLength = 4, baseSeed = 3252)



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

ens3a <- caretEnsemble(test2a)
summary(ens3a)


# Test baseSeed is preserved if specified