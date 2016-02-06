library(randomForest)
library(rpart)
library(caret)
library(plyr)
library(e1071)

# Build test data
data(iris)
Y.reg <- iris[, 1]
X.reg <- model.matrix(~ ., iris[, -1])
X.class <- X.reg
Y.class <- factor(ifelse(iris$Sepal.Length <= 6.2, "No", "Yes"))

# Reusable control
myControl_reg <- trainControl(
  method = "cv", number = 10, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  classProbs = FALSE, returnResamp = "final",
  returnData = TRUE)
myControl_class <- trainControl(
  method = "cv", number = 10, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  summaryFunction=twoClassSummary,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE)

# Regression
set.seed(482)
suppressWarnings({
  models.reg <- caretList(
    x = X.reg,
    y = Y.reg,
    methodList=c("rf", "glm", "rpart", "treebag"),
    trControl=myControl_reg
  )
})

# Classification
set.seed(482)
suppressWarnings({
  models.class <- caretList(
    x = X.class,
    y = Y.class,
    metric="ROC",
    methodList=c("rf", "glm", "rpart", "treebag"),
    trControl=myControl_class
  )
})

# Save
devtools::use_data(
  models.reg, models.class,
  Y.reg, Y.class,
  X.reg, X.class,
  overwrite=TRUE
  )
