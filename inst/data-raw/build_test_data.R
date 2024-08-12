# Build test data
data(iris)
Y.reg <- iris[, 1L]
X.reg <- model.matrix(~., iris[, -1L])
X.class <- X.reg
Y.class <- factor(ifelse(iris$Sepal.Length <= 6.2, "No", "Yes"))

# Regression
set.seed(482L)
models.reg <- caretEnsemble::caretList(
  x = X.reg,
  y = Y.reg,
  methodList = c("rf", "glm", "rpart", "treebag")
)

# Classification
set.seed(482L)
models.class <- caretEnsemble::caretList(
  x = X.class,
  y = Y.class,
  methodList = c("rf", "glm", "rpart", "treebag")
)

# Save
usethis::use_data(
  models.reg,
  models.class,
  Y.reg, Y.class,
  X.reg, X.class,
  overwrite = TRUE
)
