<<<<<<< HEAD
# Test optimizers
=======
# ## Test optimizers
#
#
# set.seed(442)
# library(caret)
#
#
# train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3,
#                      noiseVars = 10, corrVars = 4, corrValue = 0.6)
# test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3,
#                     noiseVars = 10, corrVars = 4, corrValue = 0.6)
#
# # Regression
# myControl2 = trainControl(method = "cv", number = 3, repeats = 1,
#                           p = 0.75, savePrediction = TRUE,
#                           returnResamp = "final",
#                           returnData = TRUE, verboseIter = FALSE)
#
#
# #weights1 <- greedOptRMSE(predobs$preds, predobs$obs)
# #weights2 <- qpOptRMSE(predobs$preds, predobs$obs)
#
#
# #######################
# # Classification models
# ########################
#
# # Specify controls
# myControl = trainControl(method = "cv", number = 3, repeats = 1,
#                          p = 0.75, savePrediction = TRUE,
#                          summaryFunction = twoClassSummary,
#                          classProbs = TRUE, returnResamp = "final",
#                          returnData = TRUE, verboseIter = FALSE)
#
#
#
# context("Test optimization procedure for AUC")
# weights1 <- safeOptAUC(predobs$preds, as.integer(predobs$obs))
# weights3 <- greedOptAUC(predobs$preds, predobs$obs)

# #Normalize and name weights
# weights1 <- weights1/sum(weights1)
# weights3 <- weights3/sum(weights3)
# expect_equal(weights1, weights3)
>>>>>>> 6d5c0db380c4ecee692f26e2af30ec9765ba6494

