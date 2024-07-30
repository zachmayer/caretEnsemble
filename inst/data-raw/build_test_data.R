library(caret)
library(plyr)
library(caretEnsemble)

# Build test data
data(iris)
Y.reg <- iris[, 1L]
X.reg <- model.matrix(~., iris[, -1L])
X.class <- X.reg
Y.class <- factor(ifelse(iris$Sepal.Length <= 6.2, "No", "Yes"))

# Reusable control
myControl_reg <- trainControl(
  method = "cv",
  number = 10L,
  p = 0.75,
  savePrediction = TRUE,
  classProbs = FALSE,
  returnResamp = "final",
  returnData = TRUE
)

myControl_class <- trainControl(
  method = "cv",
  number = 10L,
  p = 0.75,
  savePrediction = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  returnResamp = "final",
  returnData = TRUE
)

# Regression
set.seed(482L)
models.reg <- caretList(
  x = X.reg,
  y = Y.reg,
  methodList = c("rf", "glm", "rpart", "treebag"),
  trControl = myControl_reg
)

# Classification
set.seed(482L)
models.class <- caretList(
  x = X.class,
  y = Y.class,
  metric = "ROC",
  methodList = c("rf", "glm", "rpart", "treebag"),
  trControl = myControl_class
)

# Save
usethis::use_data(
  models.reg,
  models.class,
  Y.reg, Y.class,
  X.reg, X.class,
  overwrite = TRUE
)
