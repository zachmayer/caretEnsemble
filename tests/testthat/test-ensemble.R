
library(caret)
library(randomForest)

#############################################################################
context("Test errors and warnings")
#############################################################################
test_that("Ensembling fails with no CV", {
  my_control <- trainControl(method="none", savePredictions=TRUE)
  expect_error(expect_warning(trControlCheck(my_control)))
})

#############################################################################
context("Test metric and residual extraction")
#############################################################################

test_that("We can extract metrics", {
  data(iris)
  mod <- train(iris[,1:2], iris[,3], method="lm", trControl=trainControl(method="cv", number=3, savePredictions=TRUE))
  m1 <- getMetric(mod, "RMSE")
  m2 <- getMetric.train(mod, "RMSE")
  m3 <- getRMSE(mod)
  m4 <-  getRMSE.train(mod)
  expect_equal(m1, m2)
  expect_equal(m1, m3)
  expect_equal(m1, m4)

  m1 <- getMetricSD(mod, "RMSE")
  m2 <- getMetricSD.train(mod, "RMSE")
  expect_equal(m1, m2)
})

test_that("We can extract resdiuals from caretEnsemble objects", {
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))

  ens <- caretEnsemble(models.class)
  r <- residuals(ens)
  expect_is(r, "numeric")
  expect_equal(length(r), 150)

  ens <- caretEnsemble(models.reg)
  r <- residuals(ens)
  expect_is(r, "numeric")
  expect_equal(length(r), 150)
})

test_that("We can extract resdiuals from train objects", {
  data(iris)
  mod <- train(iris[,1:2], iris[,3], method="lm", trControl=trainControl(method="cv", number=3, savePredictions=TRUE))
  r <- residuals(mod)
  expect_is(r, "numeric")
  expect_equal(length(r), 150)

  #NOT YET SUPPORTED
#   y <- factor(ifelse(iris[,5]=="versicolor", "yes", "no"))
#   mod <- train(iris[,1:2], y, method="glm", trControl=trainControl(method="cv", number=3, savePredictions=TRUE))
#   r <- residuals(mod)
#   expect_is(r, "numeric")
#   expect_equal(length(r), 150)
})

#############################################################################
context("Does ensembling and prediction work?")
#############################################################################

test_that("We can ensemble regression models", {
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  ens.reg <- caretEnsemble(models.reg, iter=1000)
  expect_that(ens.reg, is_a("caretEnsemble"))
  pred.reg <- predict(ens.reg)
  pred.reg2 <- predict(ens.reg, keepNA=TRUE)
  pred.reg3 <- predict(ens.reg, se=TRUE, keepNA=TRUE)
  pred.reg4 <- predict(ens.reg, se=TRUE, keepNA=FALSE)
  expect_equal(pred.reg, pred.reg2)
  expect_equal(pred.reg3, pred.reg4)
  expect_equal(pred.reg, pred.reg3$pred)
  expect_warning(predict(ens.reg, return_weights="BOGUS"))
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
  ens.class <- caretEnsemble(models.class, iter=1000)
  expect_that(ens.class, is_a("caretEnsemble"))
  pred.class <- predict(ens.class)
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)

  #Check different cases
  p1 <- predict(ens.reg, return_weights=TRUE, se=FALSE, keepNA=FALSE)
  p2 <- predict(ens.reg, return_weights=TRUE, se=TRUE, keepNA=FALSE)
  p3 <- predict(ens.reg, return_weights=FALSE, se=FALSE, keepNA=FALSE)
  p4 <- predict(ens.reg, return_weights=FALSE, se=TRUE, keepNA=FALSE)
  p5 <- predict(ens.reg, return_weights=TRUE, se=FALSE, keepNA=TRUE)
  p6 <- predict(ens.reg, return_weights=TRUE, se=TRUE, keepNA=TRUE)
  p7 <- predict(ens.reg, return_weights=FALSE, se=FALSE, keepNA=TRUE)
  p8 <- predict(ens.reg, return_weights=FALSE, se=TRUE, keepNA=TRUE)

  #Check preds
  #I don't like how much the output data structure varies, depending on the input
  #I feel like it maybe should be a single lists, with none of this
  #swapping between list, data.fame, and vector output
  expect_true(all.equal(p1$preds, p2$preds$pred))
  expect_true(all.equal(p1$preds, p3))
  expect_true(all.equal(p1$preds, p4$pred))
  expect_true(all.equal(p1$preds, p5$preds))
  expect_true(all.equal(p1$preds, p6$preds$pred))
  expect_true(all.equal(p1$preds, p7))
  expect_true(all.equal(p1$preds, p8$pred))

  #Check weights
  expect_true(all.equal(p1$weight, p2$weight))
  expect_is(p3, "numeric")
  expect_null(p4$weight)
  expect_true(all.equal(p1$weight[1,,drop=FALSE], p5$weight))
  expect_true(all.equal(p1$weight[1,,drop=FALSE], p6$weight))
  expect_is(p7, "numeric")
  expect_null(p8$weight)
})

#############################################################################
context("Does ensembling work with models with differing predictors")
#############################################################################

test_that("We can ensemble models of different predictors", {
  skip_on_cran()
  data(iris)
  Y.reg <- iris[, 1]
  X.reg <- model.matrix(~ ., iris[, -1])
  mseeds <- vector(mode = "list", length = 12)
  for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
  mseeds[[12]] <- sample.int(1000, 1)
  myControl <- trainControl(method = "cv", number = 10, repeats = 1,
                           p = 0.75, savePrediction = TRUE,
                           classProbs = FALSE, returnResamp = "final",
                           returnData = TRUE, seeds = mseeds)
  set.seed(482)
  glm1 <- train(x = X.reg[, c(-1, -2, -6)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482)
  glm2 <- train(x = X.reg[, c(-1, -3, -6)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482)
  glm3 <- train(x = X.reg[, c(-1, -2, -3, -6)], y = Y.reg, method = "glm", trControl = myControl)
  set.seed(482)
  glm4 <- train(x = X.reg[, c(-1, -4, -6)], y = Y.reg, method = "glm", trControl = myControl)

  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- "caretList"
  ensNest <- caretEnsemble(nestedList, iter=1000)
  expect_that(ensNest, is_a("caretEnsemble"))
  pred.nest <- predict(ensNest, newdata = X.reg)
  expect_true(is.numeric(pred.nest))
  expect_true(length(pred.nest)==150)
})

# context("Does ensembling work with missingness")
