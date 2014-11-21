# ## Test optimizers

set.seed(442)
library('caret')
library('rpart')
library('gbm')
library('kernlab')

train <- twoClassSim(
  n = 1000, intercept = -8, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(
  n = 1500, intercept = -7, linearVars = 3,
  noiseVars = 10, corrVars = 4, corrValue = 0.6)

myList <- list(
  rf1=caretModelSpec(),
  rf2=caretModelSpec(method='rf', tuneLength=5),
  caretModelSpec(method='rpart'),
  caretModelSpec(method='knn', tuneLength=10),
  caretModelSpec(method = "glm")
)

myControl = trainControl(
  method = "cv", number = 3, repeats = 1,
  p = 0.75, savePrediction = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, returnResamp = "final",
  returnData = TRUE, verboseIter = FALSE)

myCL <- caretList(
  x = train[, -23],
  y = train[, "Class"],
  metric = "ROC",
  trControl = myControl,
  tuneList = myList)


context("Test optimizer passing to caretEnsemble correctly")

test_that("Test that optFUN does not take random values", {
  expect_error(caretEnsemble(myCL, optFUN = randomAUC))
  expect_error(caretEnsemble(myCL, optFUN = noAUC))
})


load(system.file("testdata/stuGradMod.rda",
                 package="caretEnsemble", mustWork=TRUE))

set.seed(3425)

ctrl <- trainControl(method = "cv",
                     number = 5, classProbs = TRUE, savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

out <- caretList(
  x = rbind(modeldat2$traindata$preds, modeldat2$testdata$preds),
  y = factor(c(modeldat2$traindata$class,modeldat2$testdata$class)),
  trControl = ctrl,
  tuneLength = 3,
  methodList = c("knn", "nb", "lda", "nnet"))

studentEns1 <- caretEnsemble(out, optFUN = safeOptAUC, iter = 50)
studentEns2 <- caretEnsemble(out, optFUN = greedOptAUC, iter = 50)
studentEns3 <- caretEnsemble(out)


context("Safe fails")


load(system.file("testdata/stuGradMod.rda",
                 package="caretEnsemble", mustWork=TRUE))

set.seed(3425)

ctrl <- trainControl(method = "cv",
                     number = 5, classProbs = TRUE, savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

out <- caretList(
  x = modeldat2$traindata$preds,
  y = modeldat2$traindata$class,
  trControl = ctrl,
  tuneLength = 3,
  methodList = c("knn", "nb", "lda", "nnet"))

studentEns1 <- caretEnsemble(out, optFUN = safeOptAUC, iter = 500)
studentEns2 <- caretEnsemble(out, optFUN = greedOptAUC, iter = 500)
studentEns3 <- caretEnsemble(out)

#
# ens1 <- caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200)
# ens2 <- caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200)
#
# ens1 <- caretEnsemble(myCL, optFUN = safeOptAUC, iter = 2000)
# ens2 <- caretEnsemble(myCL, optFUN = greedOptAUC, iter = 2000)
#
#
# ptm <- proc.time()
# ens1 <- caretEnsemble(myCL, optFUN = safeOptAUC, iter = 8000)
# proc.time() - ptm
#
# ptm <- proc.time()
# ens1 <- caretEnsemble(myCL, optFUN = greedOptAUC, iter = 8000)
# proc.time() - ptm
#


