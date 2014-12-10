## ----, echo=TRUE, results='hide'-----------------------------------------
suppressMessages(library('caret'))
suppressMessages(library('mlbench'))
suppressMessages(library('pROC'))

## ----, echo=TRUE, results='hide'-----------------------------------------
#Adapted from the caret vignette
library('caret')
library('mlbench')
library('pROC')
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method='boot',
  number=25,
  savePredictions=TRUE,
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
  )

## ----, echo=TRUE, results='hide', warning=FALSE--------------------------
library('rpart')
library('caretEnsemble')
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c('glm', 'rpart')
  )

