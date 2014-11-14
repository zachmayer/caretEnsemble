## ----, echo=TRUE---------------------------------------------------------
#Adapted from the caret vignette
suppressMessages(library('caret'))
suppressMessages(library('mlbench'))
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

## ----, echo=TRUE, fig.show='hold'----------------------------------------
suppressMessages(library('pROC'))
suppressMessages(library('rpart'))
set.seed(42)
suppressWarnings(model_glm <- train(
  Class ~ ., 
  data=training, 
  method='glm', 
  trControl=my_control,
  metric='ROC'))
suppressWarnings(model_rpart <- train(
  Class ~ ., 
  data=training, 
  method='rpart', 
  trControl=my_control, 
  metric='ROC'))
model_list <- list(glm=model_glm, rpart=model_rpart)
xyplot(resamples(model_list))

## ----, echo=TRUE---------------------------------------------------------
modelCor(resamples(model_list))

## ----, echo=TRUE, results='hide'-----------------------------------------
suppressMessages(library('caretEnsemble'))
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

## ----, echo=TRUE---------------------------------------------------------
summary(greedy_ensemble)

## ----, echo=TRUE, results='hide'-----------------------------------------
suppressMessages(library('caTools'))
model_preds <- lapply(model_list, predict, newdata=testing, type='prob')
model_preds <- lapply(model_preds, function(x) x[,'M'])
model_preds <- data.frame(model_preds)
suppressMessages(ens_preds <- predict(greedy_ensemble, newdata=testing))
model_preds$ensemble <- ens_preds

## ----, echo=TRUE---------------------------------------------------------
colAUC(model_preds, testing$Class)

## ----, echo=TRUE, results='hide'-----------------------------------------
varImp(greedy_ensemble)

## ----, echo=FALSE, results='asis'----------------------------------------
knitr::kable(varImp(greedy_ensemble))

## ----, echo=TRUE---------------------------------------------------------
x_train <- as.matrix(training[,-61])
x_test <- as.matrix(testing[,-61])
y_train <- training[,61]
y_test <- testing[,61]
suppressWarnings({
  model_list2 <- buildModels(
  methodList=c('glm', 'rpart'), 
  control=my_control,
  x=x_train, 
  y=y_train,
  baseSeed=42
)})

## ----, echo=TRUE---------------------------------------------------------
summary(caretEnsemble(model_list2))

## ----, echo=TRUE---------------------------------------------------------
suppressMessages(library('MASS'))
suppressWarnings({
  model_list2[['lda']] <- train(
    x_train, 
    y_train,
    method='lda', 
    trControl=my_control
    )
  })
greedy_ensemble2 <- caretEnsemble(model_list2)
summary(greedy_ensemble2)

## ----, echo=TRUE, results='hide'-----------------------------------------
suppressMessages(library('caTools'))
model_preds2 <- lapply(model_list2, predict, newdata=x_test, type='prob')
model_preds2 <- lapply(model_preds2, function(x) x[,'M'])
model_preds2 <- data.frame(model_preds2)
suppressMessages(ens_preds2 <- predict(greedy_ensemble2, newdata=x_test))
model_preds2$ensemble <- ens_preds2

## ----, echo=TRUE---------------------------------------------------------
colAUC(model_preds2, testing$Class)

## ----, echo=TRUE, results='hide'-----------------------------------------
glm_ensemble <- caretStack(
  model_list, 
  method='glm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(glm_ensemble, newdata=testing, type='prob')$M

## ----, echo=TRUE, reuslts='show'-----------------------------------------
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds3, testing$Class)
CF/sum(CF)

## ----, echo=TRUE, results='hide'-----------------------------------------
suppressMessages(library('gbm'))
gbm_ensemble <- caretStack(
  model_list, 
  method='gbm',
  verbose=FALSE,
  tuneLength=10,
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds4 <- model_preds
model_preds4$ensemble <- predict(gbm_ensemble, newdata=testing, type='prob')$M

## ----, echo=TRUE---------------------------------------------------------
colAUC(model_preds4, testing$Class)

