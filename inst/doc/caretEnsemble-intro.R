## ---- echo=FALSE, results="hide"-----------------------------------------
suppressMessages(library("caret"))
suppressMessages(library("mlbench"))
suppressMessages(library("pROC"))

## ---- echo=TRUE, results="hide"------------------------------------------
#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain, ]
testing <- Sonar[-inTrain, ]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
  )

## ---- echo=TRUE, results="hide", warning=FALSE---------------------------
library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
  )

## ---- echo=TRUE, results="hide"------------------------------------------
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

## ---- echo=FALSE, results="asis"-----------------------------------------
knitr::kable(p)

## ---- echo=FALSE, results="hide", warning=FALSE--------------------------
suppressMessages(library("mlbench"))
suppressMessages(library("randomForest"))
suppressMessages(library("nnet"))

## ---- echo=TRUE, results="hide", warning=FALSE---------------------------
library("mlbench")
library("randomForest")
library("nnet")
model_list_big <- caretList(
  Class~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)

## ---- echo=TRUE, fig.show="hold"-----------------------------------------
xyplot(resamples(model_list))

## ---- echo=TRUE----------------------------------------------------------
modelCor(resamples(model_list))

## ---- echo=TRUE----------------------------------------------------------
greedy_ensemble <- caretEnsemble(
  model_list,
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
    ))
summary(greedy_ensemble)

## ---- echo=TRUE----------------------------------------------------------
library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[, "M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)

## ---- echo=TRUE, results="hide"------------------------------------------
varImp(greedy_ensemble)

## ---- echo=FALSE, results="asis"-----------------------------------------
knitr::kable(varImp(greedy_ensemble))

## ---- echo=TRUE----------------------------------------------------------
glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, testing$Class)
CF/sum(CF)

## ---- echo=FALSE, results="hide"-----------------------------------------
suppressMessages(library("gbm"))
suppressMessages(library("plyr"))

## ---- echo=TRUE----------------------------------------------------------
library("gbm")
gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
colAUC(model_preds3, testing$Class)
