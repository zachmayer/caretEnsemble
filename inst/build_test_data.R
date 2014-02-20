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

save(models_reg, Y.reg, X.reg, file="data/models_reg.RData")

rm(i, glm1, lm1, nn1, rf1)
rm(list = ls())


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
nnet1 <- train(x = X.class, y = Y.class, method = 'nnet', trControl = myControl)
set.seed(482)
bag1 <- train(x = X.class, y = Y.class, method = 'treebag', trControl = myControl)
set.seed(482)
nn1 <- train(x = X.class, y = Y.class, method = 'knn', trControl = myControl)

models_class <- list(rf1, glm1, svm1, nnet1, bag1, nn1)

save(models_class, Y.class, X.class, file="data/models_class.RData")

rm(list=ls())

