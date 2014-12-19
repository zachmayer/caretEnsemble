# Build test data

data(iris)
Y.reg <- iris[, 1]
X.reg <- model.matrix(~ ., iris[, -1])


# Regression

mseeds <- vector(mode = "list", length = 11)
for(i in 1:10) mseeds[[i]] <- sample.int(1000, 3)
mseeds[[11]] <- sample.int(1000, 1)
myControl = trainControl(method = "cv", number = 10, repeats = 1,
                      p = 0.75, savePrediction = TRUE,
                      classProbs = FALSE, returnResamp = "final",
                      returnData = TRUE, seeds = mseeds)

set.seed(482)
rf1 <- train(x = X.reg, y = Y.reg, method = 'rf', trControl = myControl)
set.seed(482)
lm1 <- train(x = X.reg[, c(-1, -6)], y = Y.reg, method = 'lm', trControl = myControl)
set.seed(482)
glm1 <- train(x = X.reg[, c(-1, -6)], y = Y.reg, method = 'glm', trControl = myControl)
set.seed(482)
nn1 <- train(x = X.reg, y = Y.reg, method = 'knn', trControl = myControl)

models_reg <- list(rf1, lm1, glm1, nn1)
class(models_reg) <- 'caretList'

devtools::use_data(models_reg, Y.reg, X.reg, overwrite=TRUE)

rm(i, glm1, lm1, nn1, rf1)
rm(list = ls(all=TRUE))


data(iris)
X.class <- model.matrix(~ ., iris[, -1])
Y.class <- ifelse(iris$Sepal.Length <= 6.2, "No", "Yes")
Y.class <- factor(Y.class)

mseeds <- vector(mode = "list", length = 11)
for(i in 1:10) mseeds[[i]] <- sample.int(1000, 9)
mseeds[[11]] <- sample.int(1000, 1)


myControl = trainControl(method = "cv", number = 10, repeats = 1,
                         p = 0.75, savePrediction = TRUE,
                         classProbs = TRUE, seeds = mseeds)

set.seed(482)
rf1 <- train(x = X.class, y = Y.class, method = 'rf', trControl = myControl)
set.seed(482)
glm1 <- train(x = X.class[, c(-1, -6)], y = Y.class, method = 'glm', trControl = myControl)
set.seed(482)
svm1 <- train(x = X.class, y = Y.class, method = 'svmRadial', trControl = myControl)
set.seed(482)
nnet1 <- train(x = X.class, y = Y.class, method = 'nnet', trControl = myControl, trace=FALSE)
set.seed(482)
bag1 <- train(x = X.class, y = Y.class, method = 'treebag', trControl = myControl)
set.seed(482)
nn1 <- train(x = X.class, y = Y.class, method = 'knn', trControl = myControl)

models_class <- list(rf1, glm1, svm1, nnet1, bag1, nn1)
class(models_class) <- 'caretList'

devtools::use_data(models_class, Y.class, X.class, overwrite=TRUE)

rm(list=ls())

#caretList train/test data
set.seed(442)
train <- twoClassSim(
  n = 1000, intercept = -8, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(
  n = 1500, intercept = -7, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
devtools::use_data(train, test, overwrite=TRUE)

myList <- list(
  rf1=caretModelSpec(),
  rf2=caretModelSpec(method='rf', tuneLength=5),
  caretModelSpec(method='rpart'),
  caretModelSpec(method='knn', tuneLength=10),
  caretModelSpec(method = "glm")
)

myControl = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

myCL <- caretList(
  x = train[, -23],
  y = train[, "Class"],
  metric = "ROC",
  trControl = myControl,
  tuneList = myList)
devtools::use_data(myList, myControl, myCL, overwrite=TRUE)


## Uglier data with class imbalance

# devtools::install_github("jknowles/EWStools") # to get the data
# library(EWStools)
# data(EWStestData)
#
# set.seed(3425)
# ctrl <- trainControl(method = "cv",
#                      number = 3, classProbs = TRUE, savePredictions = TRUE,
#                      summaryFunction = twoClassSummary)
#
# modeldat2 <- assembleData(fulldat[1:300, ], class = "y", p = 0.5,
#                          predvars = names(fulldat)[-1], classification = TRUE)
#
# out <- caretList(
#   x = modeldat2$traindata$preds,
#   y = modeldat2$traindata$class,
#   trControl = ctrl,
#   tuneLength = 3,
#   methodList = c("knn", "nb", "lda"),
#   tuneList = list(nnet=caretModelSpec(method='nnet', trace=FALSE))
#   )
#
#  studentEns <- caretEnsemble(out)
#
# devtools::use_data(modeldat2, studentEns, overwrite=TRUE)
