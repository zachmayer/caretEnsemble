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

context("Test optimizers function similarly under normal conditions")

test_that("safe and greedy optimizers get same result in the limit", {
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC),
                    caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
                    caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))

})

context("Test more difficult cases")

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

studentEns1 <- caretEnsemble(out, optFUN = safeOptAUC, iter = 200)
studentEns2 <- caretEnsemble(out, optFUN = greedOptAUC, iter = 200)
studentEns3 <- caretEnsemble(out)

test_that("safe and greedy optimizers get same result in the limit", {
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 200),
                   caretEnsemble(myCL, optFUN = greedOptAUC, iter = 200))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
                   caretEnsemble(myCL, optFUN = greedOptAUC))
  expect_identical(caretEnsemble(myCL, optFUN = safeOptAUC, iter = 100),
                   caretEnsemble(myCL, optFUN = greedOptAUC))


})


context("Warnings and fallbacks in degenerate cases")

set.seed(4876)

ctrl <- trainControl(method = "cv",
                     number = 5, classProbs = TRUE, savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

out <- caretList(
  x = modeldat2$traindata$preds,
  y = modeldat2$traindata$class,
  trControl = ctrl,
  tuneLength = 3,
  methodList = c("knn", "nb", "lda", "nnet"))

predobs <- makePredObsMatrix(out)

wghts1 <- safeOptAUC(predobs$preds, predobs$obs)
wghts2 <- greedOptAUC(predobs$preds, predobs$obs)

test_that("Warnings and messages are correct", {
  expect_warning(safeOptAUC(predobs$preds, predobs$obs), "Returning best model")
  expect_message(greedOptAUC(predobs$preds, predobs$obs), "Try more iterations")
  expect_warning(caretEnsemble(out, optFUN = safeOptAUC), "Returning best model")
  expect_message(caretEnsemble(out, optFUN = greedOptAUC), "Try more iterations")
})

test_that("safe and greed return different results", {
  expect_false(identical(wghts1, wghts2))
  expect_equal(wghts1, c(0, 0, 0, 1))
  expect_equal(wghts2, c(4, 88, 0, 8))
})

test_that("Correct results propogate to full caretEnsemble object", {
  ens1 <- caretEnsemble(out, optFUN = safeOptAUC)
  ens2 <- caretEnsemble(out, optFUN = greedOptAUC)
  expect_false(identical(ens1, ens2))
  expect_equivalent(ens1$weights, 1)
  expect_equivalent(length(ens1$weights), 1)
  expect_equivalent(ens2$weights, c(0.04, 0.88, 0.08))
  expect_equivalent(length(ens2$weights), 3)
})
