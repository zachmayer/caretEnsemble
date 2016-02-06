
context("Does variable importance work?")
library(caret)
library(randomForest)

data(models.reg)
data(X.reg)
data(Y.reg)

data(models.class)
data(X.class)
data(Y.class)

test_that("We can get variable importance in ensembles", {
  skip_on_cran()
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, trControl=trainControl(method="none"))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(method="none"))
  expect_is(varImp(ens.class), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg), "data.frame")
  expect_is(varImp(ens.reg, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("We get warnings when scale is set to FALSE and weight is TRUE", {
  skip_on_cran()
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, trControl=trainControl(method="none"))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(method="none"))
  i <- varImp(ens.reg, scale = FALSE, weight = TRUE)
  i <- varImp(ens.class, scale = FALSE, weight = TRUE)
  i <- varImp(ens.reg, scale = FALSE, weight = TRUE)
  i <- varImp(ens.class, scale = FALSE, weight = TRUE)
  i <- varImp(ens.reg, scale = FALSE)
  i <- varImp(ens.class, scale = FALSE)
  i <- varImp(ens.reg, scale = FALSE)
  i <- varImp(ens.class, scale = FALSE)
})

test_that("We get the right dimensions back", {
  skip_on_cran()
  ncol1 <- 5
  ncol2 <- 4
  nrow1 <- 6
  nrow2 <- 6
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, trControl=trainControl(method="none"))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(method="none"))
  expect_equal(ncol(varImp(ens.class)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = FALSE)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = TRUE)), ncol1)
  expect_equal(ncol(varImp(ens.reg)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = FALSE)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = TRUE)), ncol2)
  expect_equal(nrow(varImp(ens.class)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = FALSE)), nrow1)
  expect_equal(nrow(varImp(ens.class, weight = TRUE)), nrow1)
  expect_equal(nrow(varImp(ens.reg)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = FALSE)), nrow2)
  expect_equal(nrow(varImp(ens.reg, weight = TRUE)), nrow2)
})

context("Do metric extraction functions work as expected")

test_that("Metric is used correctly", {
  skip_on_cran()
  set.seed(2239)

  #Make ensemble
  ens.class <- caretEnsemble(
    models.class,
    metric="ROC",
    trControl=trainControl(
      number=2,
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))

  #Make ensemble
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(number=2))

  #Get an incorrect metric
  expect_error(getMetric.train(ens.class$models[[3]], metric = "RMSE"))
  expect_error(getMetric.train(ens.reg$models[[2]], metric = "ROC"))

  #Correct metric
  expect_equal(getMetric.train(ens.class$models[[1]], metric = "ROC"), 0.9293333, tol = 0.025)
  expect_equal(getMetric.train(ens.class$models[[2]], metric = "ROC"), 0.9406667, tol = 0.025)
  expect_equal(getMetric.train(ens.class$models[[3]], metric = "ROC"), 0.8826667, tol = 0.025)
  expect_equal(getMetric.train(ens.class$models[[4]], metric = "ROC"), 0.9153333, tol = 0.025)

  #Correct metric
  expect_equal(getMetric.train(ens.reg$models[[1]], metric = "RMSE"), 0.3146584, tol = 0.025)
  expect_equal(getMetric.train(ens.reg$models[[2]], metric = "RMSE"), 0.439482, tol = 0.025)
  expect_equal(getMetric.train(ens.reg$models[[3]], metric = "RMSE"), 0.3361409, tol = 0.025)

  #Correct metric
  expect_equal(getMetricSD.train(ens.class$models[[1]], metric = "ROC"), 0.05897897, tol = 0.025)
  expect_equal(getMetricSD.train(ens.class$models[[2]], metric = "ROC"), 0.05196865, tol = 0.025)
  expect_equal(getMetricSD.train(ens.class$models[[3]], metric = "ROC"), 0.107207, tol = 0.025)
  expect_equal(getMetricSD.train(ens.class$models[[4]], metric = "ROC"), 0.07554248, tol = 0.025)

  #Correct metric
  expect_equal(getMetricSD.train(ens.reg$models[[1]], metric = "RMSE"), 0.05839238, tol = 0.025)
  expect_equal(getMetricSD.train(ens.reg$models[[2]], metric = "RMSE"), 0.08709325, tol = 0.025)
  expect_equal(getMetricSD.train(ens.reg$models[[3]], metric = "RMSE"), 0.06942881, tol = 0.025)
})

context("Testing caretEnsemble generics")

test_that("No errors are thrown by a generics for ensembles", {
  skip_on_cran()
  set.seed(2239)
  ens.class <- caretEnsemble(
    models.class,
    metric="ROC",
    trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
    ))
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(number=2))
  expect_output(summary(ens.class), "ROC")
  expect_output(summary(ens.reg), "RMSE")

  expect_is(plot(ens.class), "ggplot")
  expect_is(plot(ens.reg), "ggplot")
  expect_is(plot(ens.reg$models[[2]]), "trellis")
  tp <- plot(ens.class)
  tp2 <- plot(ens.reg)
  expect_equal(nrow(tp$data), 4)
  expect_equal(nrow(tp2$data), 3)
  expect_equal(tp$data$method, names(ens.class$models))
  expect_equal(tp2$data$method, names(ens.reg$models))
  expect_warning(fort1 <- fortify(ens.class))
  expect_warning(fort2 <- fortify(ens.reg))
  expect_is(fort1, "data.frame")
  expect_is(fort2, "data.frame")
  expect_equal(nrow(fort1), 150)
  expect_equal(nrow(fort2), 150)
  expect_equal(ncol(fort1), 10)
  expect_equal(ncol(fort2), 10)
  expect_true(all(names(fort1) %in% names(fort2)))

  test_plot_file <- "caretEnsemble_test_plots.png"
  png(test_plot_file)
  p1 <- autoplot(ens.class)
  p2 <- autoplot(ens.reg)
  p3 <- autoplot(ens.class, xvars=c("Petal.Length", "Petal.Width"))
  p4 <- autoplot(ens.reg, xvars=c("Petal.Length", "Petal.Width"))
  expect_error(autoplot(ens.reg$models[[1]]))
  dev.off()
  expect_true(file.exists(test_plot_file))
  unlink(test_plot_file)
})

context("Residual extraction")

test_that("Residuals provided by residuals are proper for ensemble objects", {
  skip_on_cran()
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, trControl=trainControl(method="none"))
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(method="none"))
  residTest <- residuals(ens.class)
  residTest2 <- residuals(ens.reg)
  obs1 <- ifelse(Y.class == "No", 0, 1)
  obs2 <- Y.reg
  predTest <- predict(ens.class)
  predTest2 <- predict(ens.reg)
  #expect_identical(residTest, obs1 - predTest)
  #expect_identical(residTest2, obs2 - predTest2)
  expect_false(identical(residTest2, predTest2 -obs2))

  mr1 <- multiResiduals(ens.class)
  mr2 <- multiResiduals(ens.reg)
  expect_identical(names(mr1), names(mr2))
  expect_identical(names(mr1), c("method", "id", "yhat", "resid", "y"))
  expect_equal(nrow(mr1), 150 * length(ens.class$models))
  expect_equal(nrow(mr2), 150 * length(ens.reg$models))
  expect_equal(ncol(mr1), ncol(mr2))
  mr1 <- mr1[order(mr1$method, mr1$id), ]
  mr2 <- mr2[order(mr2$method, mr2$id), ]
  mr2.tmp1 <- residuals(ens.reg$models[[1]])
  attributes(mr2.tmp1) <- NULL
  mr2.tmp2 <- residuals(ens.reg$models[[2]])
  expect_true(identical(round(mr2[mr2$method == "glm", "resid"], 5), round(mr2.tmp1, 5)))
  #expect_true(identical(round(mr2[mr2$method == "rpart", "resid"], 5), round(mr2.tmp2, 5)))

  #I think the factors are backward somewhere in here
  #Also, caret doesn't yet support residuals for classification
  #   mr_class_wide <- as.data.frame(lapply(ens.class$models, residuals))
  #   names(mr_class_wide) <- lapply(ens.class$models, function(x) x$method)
  #   mr_class_long <- reshape(mr_class_wide, direction = "long", varying = names(mr_class_wide),
  #                            v.names = "resid", timevar = "method", times = names(mr_class_wide))
  #   expect_equal(mr_class_long[order(mr_class_long$method, mr_class_long$id),"resid"], -1*mr1[order(mr1$method, mr1$id),"resid"])

  mr_reg_wide <- as.data.frame(lapply(ens.reg$models, residuals))
  names(mr_reg_wide) <- lapply(ens.reg$models, function(x) x$method)
  mr_reg_long <- reshape(mr_reg_wide, direction = "long", varying = names(mr_reg_wide),
                         v.names = "resid", timevar = "method", times = names(mr_reg_wide))
  expect_equal(mr_reg_long[order(mr_reg_long$method, mr_reg_long$id), "resid"], mr2[order(mr2$method, mr2$id), "resid"])

  ens.class2 <- ens.class
  ens.reg2 <- ens.reg
  ens.class2$modelType <- ens.reg2$modelType <- NULL

  expect_equal(residuals(ens.class2), residuals(ens.class))
  expect_equal(residuals(ens.reg2), residuals(ens.reg))
})

context("Are ensembles construct accurately")

test_that("Do model results in caretEnsemble match component models - classification", {
  skip_on_cran()
  set.seed(2239)
  ens.class <- caretEnsemble(
    models.class, metric="ROC",
    trControl=trainControl(
      number=2, summaryFunction=twoClassSummary, classProbs=TRUE
      )
    )
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, trControl=trainControl(number=2))
  modres1 <- extractModRes(ens.class)
  modres2 <- extractModRes(ens.reg)

  modF <- extractModFrame(ens.class)
  modF2 <- extractModFrame(ens.reg)
  expect_true(nrow(modF) == nrow(ens.class$models[[2]]$trainingData))
  expect_true(nrow(modF2) == nrow(ens.reg$models[[1]]$trainingData))
})

test_that("Do model results in caretEnsemble match component models - regression", {
  skip_on_cran()
  ens.reg <- caretEnsemble(models.reg, trControl=trainControl(method="none"))
  models.class2 <- models.class[c(2:4)]
  class(models.class2) <- "caretList"
  ens.class <- caretEnsemble(models.class2, trControl=trainControl(method="none"))
  newDat <- ens.class$models[[3]]$trainingData
  newDat[2, 2] <- NA
  newDat[3, 3] <- NA
  newDat[4, 4] <- NA
  newDat <- newDat[1:10, ]
  # expect_error(predict(ens.class, newdata = newDat, return_weights=TRUE, se=FALSE)) # BUG
  expect_error(predict(ens.reg, newdata = newDat, return_weights=TRUE, se=TRUE))
  expect_error(predict(ens.reg, newdata = newDat, return_weights=FALSE, se=FALSE))
  expect_error(predict(ens.reg, newdata = newDat, return_weights=FALSE, se=TRUE))
})

#Reg tests
test_that("Prediction options are respected in regression and classification", {
  skip_on_cran()
  ens.reg <- caretEnsemble(models.reg, trControl=trainControl(method="none"))
  tests <- expand.grid(se=0:1, return_weights=0:1)
  tests <- data.frame(lapply(tests, as.logical))
  for(i in 1:nrow(tests)){

    expect_warning({
      p <- predict(
        ens.reg,
        se=tests[i, "se"],
        return_weights=tests[i, "return_weights"]
      )
    })

    if(tests[i, "se"]){
      expect_is(p, "data.frame")
      preds <- p
    } else{
      expect_is(p, "numeric")
      preds <- p
    }

    if(tests[i, "return_weights"]){
      expect_is(attr(preds, which = "weights"), "numeric")
    } else{
      expect_null(attr(preds, which = "weights"))
    }
  }

  #Class tests
  ens.class <- caretEnsemble(models.class, trControl=trainControl(method="none"))
  tests <- expand.grid(se=0:1, return_weights=0:1)
  tests <- data.frame(lapply(tests, as.logical))
  for(i in 1:nrow(tests)){
    expect_warning({
      p <- predict(
        ens.class,
        se=tests[i, "se"],
        return_weights=tests[i, "return_weights"],
        type="prob"
      )
    })

    if(tests[i, "se"]){
      expect_is(p, "data.frame")
      preds <- p
    } else{
      expect_is(p, "numeric")
      preds <- p
    }

    if(tests[i, "return_weights"]){
      expect_is(attr(preds, which = "weights"), "numeric")
    } else{
      expect_null(attr(preds, which = "weights"))
    }
  }
})
