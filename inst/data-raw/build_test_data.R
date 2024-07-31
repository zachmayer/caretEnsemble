# Build test data
data(iris)
Y.reg <- iris[, 1L]
X.reg <- model.matrix(~., iris[, -1L])
X.class <- X.reg
Y.class <- factor(ifelse(iris$Sepal.Length <= 6.2, "No", "Yes"))

# Reusable control
myControl_reg <- caret::trainControl(
  method = "cv",
  number = 10L,
  p = 0.75,
  savePrediction = TRUE,
  classProbs = FALSE,
  returnResamp = "final",
  returnData = TRUE
)

myControl_class <- caret::trainControl(
  method = "cv",
  number = 10L,
  p = 0.75,
  savePrediction = TRUE,
  summaryFunction = caret::twoClassSummary,
  classProbs = TRUE,
  returnResamp = "final",
  returnData = TRUE
)

# Regression
set.seed(482L)
models.reg <- caretEnsemble::caretList(
  x = X.reg,
  y = Y.reg,
  methodList = c("rf", "glm", "rpart", "treebag"),
  trControl = myControl_reg
)

# Classification
set.seed(482L)
models.class <- caretEnsemble::caretList(
  x = X.class,
  y = Y.class,
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
