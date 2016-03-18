#context("Does binary class selection work?")
library(caret)
library(caretEnsemble)

# library(devtools); unload(inst("caretEnsemble")); install_local('/Users/eczech/repos/misc/caretEnsemble');

data(models.class)
data(X.class)
data(Y.class)

# custom.rf <- getModelInfo('rf', regex=F)[[1]]
# custom.rf$method <- 'custom-rf'
#
# model.list <- caretList(X.class, Y.class, methodList = list(custom.rf, custom.rf, 'rf', 'rf'),
#                         trControl=trainControl(classProbs=T))
# model.ens <- caretEnsemble(model.list)
# predict(model.ens, newdata=X.class, type='prob')

library(foreach)
library(iterators)
set.seed(123)
folds <- createFolds(Y.class, k = 10, returnTrain = T)

trainModel <- function(model){
  foreach(fold=folds, i=icount())%do%{
    set.seed(123)
    X.train <- X.class[fold,]
    X.test <- X.class[-fold,]
    Y.train <- Y.class[fold]
    Y.test <- Y.class[-fold]
    fit <- model$train(X.train, Y.train, i)
    y.pred <- model$predict(fit, X.test)
    list(fit=fit, y.pred=y.pred, y.test=Y.test)
  }
}

custom.rf <- getModelInfo('rf', regex=F)[[1]]
custom.rf$method <- 'customrf'
trControl <- trainControl(savePredictions = T, classProbs = T, method='cv', number=5)
pred <- function(fit, X) predict(fit, newdata=X, type='prob')[1,]
models <- list()
models$rf <- list(train=function(X, y, i) train(X, y, method='rf', trControl=trControl), predict=pred)
models$rpart <- list(train=function(X, y, i) train(X, y, method='rpart', trControl=trControl), predict=pred)
models$crf <- list(train=function(X, y, i) train(X, y, method=custom.rf, trControl=trControl), predict=pred)

results <- list()
results$rf <- trainModel(models$rf)
results$rpart <- trainModel(models$rpart)
results$crf <- trainModel(models$crf)

ens.models <- list(
  rf=function(i) results$rf[[i]]$fit,
  rpart=function(i) results$rpart[[i]]$fit,
  crf=function(i) results$crf[[i]]$fit
)
models$ens <- list(
  train=function(X, y, i){
    m <- lapply(ens.models, function(m) m(i))
    class(m) <- "caretList"
    ens <- caretStack(m, method='glm', trControl=trainControl(savePredictions = T, method='cv', number=3))
    ens
  }, predict = function(fit, X){
    predict(fit, newdata=X, type='prob')
  }
)
options(error=traceback)
options(Debug=T)
results$ens <- trainModel(models$ens)

test_that("Ensembled classifiers do not rearrange outcome factor levels", {
  skip_on_cran()


})
