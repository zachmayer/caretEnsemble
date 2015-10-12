# library(caretEnsemble)
# library('mlbench')
# data(Sonar)
#
# my_control <- trainControl(
#   method='boot',
#   number=25,
#   savePredictions=TRUE,
#   classProbs=TRUE,
#   index=createResample(Sonar$Class, 25)
# )
#
# model_list <- caretList(
#   Class~., data=Sonar,
#   trControl=my_control,
#   methodList=c('glm', 'rpart')
# )
#
# greedy_ensemble <- caretEnsemble(model_list)
#
# list_preds <- predict(model_list, newdata = Sonar[1, ])
# ens_preds <- predict(greedy_ensemble, newdata=Sonar[1,])
# ens_preds <- predict(greedy_ensemble, newdata=rbind(Sonar[1,],Sonar[1,]))[1]
