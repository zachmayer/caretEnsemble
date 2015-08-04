
context("Does variable importance work?")
library(caret)
library(randomForest)

test_that("We can get variable importance in ensembles", {
  skip_on_cran()
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  expect_is(varImp(ens.class), "data.frame")
  expect_is(varImp(ens.class, weight = TRUE), "data.frame")
  expect_is(varImp(ens.class, scale = TRUE, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg), "data.frame")
  expect_is(varImp(ens.reg, weight = TRUE), "data.frame")
  expect_is(varImp(ens.reg, scale = TRUE, weight = TRUE), "data.frame")
})

test_that("We get warnings when scale is set to FALSE and weight is TRUE", {
  skip_on_cran()
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  gives_warning(varImp(ens.reg, scale = FALSE, weight = TRUE))
  gives_warning(varImp(ens.class, scale = FALSE, weight = TRUE))
  expect_warning(varImp(ens.reg, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE, weight = TRUE),
                 "Weighting of unscaled")
  gives_warning(varImp(ens.reg, scale = FALSE))
  gives_warning(varImp(ens.class, scale = FALSE))
  expect_warning(varImp(ens.reg, scale = FALSE),
                 "Weighting of unscaled")
  expect_warning(varImp(ens.class, scale = FALSE),
                 "Weighting of unscaled")
})

test_that("We get the right dimensions back", {
  skip_on_cran()
  ncol1 <- 6
  ncol2 <- 4
  nrow1 <- 6
  nrow2 <- 6
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  expect_equal(ncol(varImp(ens.class)), ncol1)
  expect_equal(ncol(varImp(ens.class, weight = FALSE)), ncol1-1)
  expect_equal(ncol(varImp(ens.class, weight = TRUE)), ncol1)
  expect_equal(ncol(varImp(ens.reg)), ncol2)
  expect_equal(ncol(varImp(ens.reg, weight = FALSE)), ncol2-1)
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
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  expect_error(caretEnsemble:::getRMSE.train(ens.class$models[[1]]))
  expect_error(caretEnsemble:::getRMSE.train(ens.class$models[[3]]))
  expect_error(caretEnsemble:::getMetric.train(ens.class$models[[3]], metric = "RMSE"))
  expect_error(caretEnsemble:::getAUC.train(ens.reg$models[[1]]))
  expect_error(caretEnsemble:::getAUC.train(ens.reg$models[[2]]))
  expect_error(caretEnsemble:::getMetric.train(ens.reg$models[[2]], metric = "AUC"))
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[1]]), 0.9287978, tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[2]]), 0.942959, tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[3]]), 0.9185977, tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[4]]), 0.9405823, tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[5]]), 0.9250347, tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[5]]),
               caretEnsemble:::getMetric.train(ens.class$models[[5]], metric = "AUC"), tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[4]]),
               caretEnsemble:::getMetric.train(ens.class$models[[4]], metric = "AUC"), tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[3]]),
               caretEnsemble:::getMetric.train(ens.class$models[[3]], metric = "AUC"), tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[2]]),
               caretEnsemble:::getMetric.train(ens.class$models[[2]], metric = "AUC"), tol = 0.025)
  expect_equal(caretEnsemble:::getAUC.train(ens.class$models[[1]]),
               caretEnsemble:::getMetric.train(ens.class$models[[1]], metric = "AUC"), tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[1]]), 0.3334612, tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[2]]), 0.324923, tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[3]]), 0.324923, tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[4]]), 0.3532128, tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[1]]),
               caretEnsemble:::getMetric.train(models.reg[[1]], metric = "RMSE"), tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[2]]),
               caretEnsemble:::getMetric.train(models.reg[[2]], metric = "RMSE"), tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[3]]),
               caretEnsemble:::getMetric.train(models.reg[[3]], metric = "RMSE"), tol = 0.025)
  expect_equal(caretEnsemble:::getRMSE.train(models.reg[[4]]),
               caretEnsemble:::getMetric.train(models.reg[[4]], metric = "RMSE"), tol = 0.025)
  expect_error(caretEnsemble:::getMetric.train(models.reg[[1]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetric.train(models.reg[[2]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetric.train(models.reg[[3]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetric.train(models.reg[[4]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetric.train(models.class[[1]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetric.train(models.class[[2]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetric.train(models.class[[3]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetric.train(models.class[[4]], metric = "RMSE"))
  expect_message(caretEnsemble:::getMetricSD.train(models.reg[[1]]))
  expect_message(caretEnsemble:::getMetricSD.train(models.class[[1]]))
  expect_error(caretEnsemble:::getMetricSD.train(models.reg[[1]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetricSD.train(models.reg[[2]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetricSD.train(models.reg[[3]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetricSD.train(models.reg[[4]], metric = "AUC"))
  expect_error(caretEnsemble:::getMetricSD.train(models.class[[1]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetricSD.train(models.class[[2]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetricSD.train(models.class[[3]], metric = "RMSE"))
  expect_error(caretEnsemble:::getMetricSD.train(models.class[[4]], metric = "RMSE"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[1]]), 0.05873828, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[2]]), 0.05517874, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[3]]), 0.05517874, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[4]]), 0.07023269, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[4]]),
               caretEnsemble:::getMetricSD.train(models.reg[[4]], metric = "RMSE"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[3]]),
               caretEnsemble:::getMetricSD.train(models.reg[[3]], metric = "RMSE"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[2]]),
               caretEnsemble:::getMetricSD.train(models.reg[[2]], metric = "RMSE"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.reg[[1]]),
               caretEnsemble:::getMetricSD.train(models.reg[[1]], metric = "RMSE"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[1]]), 0.0582078, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[2]]), 0.05196865, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[3]]), 0.06356099, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[4]]), 0.07360202, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[4]]),
               caretEnsemble:::getMetricSD.train(models.class[[4]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[3]]),
               caretEnsemble:::getMetricSD.train(models.class[[3]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[2]]),
               caretEnsemble:::getMetricSD.train(models.class[[2]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(models.class[[1]]),
               caretEnsemble:::getMetricSD.train(models.class[[1]], metric = "AUC"))
})

context("Metrics in student examples")

test_that("metrics work for AUC in imbalanced example", {
  skip_on_cran()
  load(system.file("testdata/studentEns.rda",
                   package="caretEnsemble", mustWork=TRUE))
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[1]]), 0.9340861, tol = 0.025)
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[2]]), 0.873687, tol = 0.025)
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[3]]), 0.8839286, tol = 0.025)
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[3]]),
               caretEnsemble:::getMetric.train(studentEns$models[[3]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[2]]),
               caretEnsemble:::getMetric.train(studentEns$models[[2]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetric.train(studentEns$models[[1]]),
               caretEnsemble:::getMetric.train(studentEns$models[[1]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[1]]), 0.04611638, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[2]]), 0.03840437, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[3]]), 0.0144596, tol = 0.025)
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[3]]),
               caretEnsemble:::getMetricSD.train(studentEns$models[[3]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[2]]),
               caretEnsemble:::getMetricSD.train(studentEns$models[[2]], metric = "AUC"))
  expect_equal(caretEnsemble:::getMetricSD.train(studentEns$models[[1]]),
               caretEnsemble:::getMetricSD.train(studentEns$models[[1]], metric = "AUC"))
})

context("Testing caretEnsemble generics")

test_that("No errors are thrown by a generics for ensembles", {
  skip_on_cran()
  load(system.file("testdata/studentEns.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  # varImp struggles with the rf in our test suite, why?
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  expect_output(summary(ens.class), "AUC")
  expect_output(summary(ens.reg), "RMSE")
  expect_output(summary(studentEns), "AUC")
  expect_is(plot(ens.class), "ggplot")
  expect_is(plot(ens.reg), "ggplot")
  expect_is(plot(ens.reg$models[[2]]), "trellis")
  tp <- plot(ens.class)
  tp2 <- plot(ens.reg)
  expect_equal(nrow(tp$data), 5)
  expect_equal(nrow(tp2$data), 2)
  expect_equal(tp$data$method, names(ens.class$weights))
  expect_equal(tp2$data$method, names(ens.reg$weights))
  fort1 <- fortify(ens.class)
  fort2 <- fortify(ens.reg)
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
  load(system.file("testdata/studentEns.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/X.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/Y.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  residTest <- residuals(ens.class)
  residTest2 <- residuals(ens.reg)
  obs1 <- ifelse(Y.class == "No", 0, 1)
  obs2 <- Y.reg
  predTest <- predict(ens.class)
  predTest2 <- predict(ens.reg)
  expect_identical(residTest, obs1 - predTest)
  expect_identical(residTest2, obs2 - predTest2)
  expect_false(identical(residTest2, predTest2 -obs2))
  expect_false(identical(residTest, predTest -obs1))
  mr1 <- multiResiduals(ens.class)
  mr2 <- multiResiduals(ens.reg)
  expect_identical(names(mr1), names(mr2))
  expect_identical(names(mr1), c("method", "id", "yhat", "resid", "y"))
  expect_equal(nrow(mr1), 150 * length(ens.class$models))
  expect_equal(nrow(mr2), 150 * length(ens.reg$models))
  expect_equal(ncol(mr1), ncol(mr2))
  mr1 <- mr1[order(mr1$method, mr1$id),]
  mr2 <- mr2[order(mr2$method, mr2$id),]
  mr2.tmp1 <- residuals(ens.reg$models[[1]])
  attributes(mr2.tmp1) <- NULL
  mr2.tmp2 <- residuals(ens.reg$models[[2]])
  expect_true(identical(round(mr2[mr2$method == "lm", "resid"], 5), round(mr2.tmp1, 5)))
  expect_true(identical(round(mr2[mr2$method == "knn", "resid"], 5), round(mr2.tmp2, 5)))

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
  expect_equal(mr_reg_long[order(mr_reg_long$method, mr_reg_long$id),"resid"], mr2[order(mr2$method, mr2$id),"resid"])

  ens.class2 <- ens.class
  ens.reg2 <- ens.reg
  ens.class2$modelType <- ens.reg2$modelType <- NULL

  expect_equal(residuals(ens.class2), residuals(ens.class))
  expect_equal(residuals(ens.reg2), residuals(ens.reg))
})

context("Does prediction method work for classification")

test_that("We can ensemble models and handle missingness across predictors", {
  skip_on_cran()
  load(system.file("testdata/models.reg.rda",
                   package="caretEnsemble", mustWork=TRUE))
  load(system.file("testdata/models.class.rda",
                   package="caretEnsemble", mustWork=TRUE))
  set.seed(2239)
  ens.class <- caretEnsemble(models.class, iter=100)
  models.subset <- models.reg[2:4]
  class(models.subset) <- "caretList"
  ens.reg <- caretEnsemble(models.subset, iter=100)
  modres1 <- caretEnsemble:::extractModRes(ens.class)
  modres2 <- caretEnsemble:::extractModRes(ens.reg)
  expect_false(identical(modres1[1, 2], max(ens.class$models[[1]]$results$Accuracy)))
  expect_false(identical(modres2[1, 2], max(ens.reg$models[[1]]$results$RMSE)))
  expect_false(identical(modres1[2, 2], max(ens.class$models[[2]]$results$Accuracy)))
  expect_false(identical(modres2[2, 2], max(ens.reg$models[[2]]$results$RMSE)))
  expect_false(identical(modres1[3, 2], max(ens.class$models[[3]]$results$Accuracy)))
  expect_false(identical(modres1[4, 2], max(ens.class$models[[4]]$results$Accuracy)))
  expect_false(identical(modres1[5, 2], max(ens.class$models[[5]]$results$Accuracy)))
  expect_identical(modres1[1, 2], caretEnsemble:::getAUC.train(ens.class$models[[1]]))
  expect_identical(modres1[2, 2], caretEnsemble:::getAUC.train(ens.class$models[[2]]))
  expect_identical(modres1[3, 2], caretEnsemble:::getAUC.train(ens.class$models[[3]]))
  expect_identical(modres1[4, 2], caretEnsemble:::getAUC.train(ens.class$models[[4]]))
  expect_identical(modres1[5, 2], caretEnsemble:::getAUC.train(ens.class$models[[5]]))
  expect_identical(modres1[1, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[1]], "AUC", which = "best"))
  expect_identical(modres1[2, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[2]], "AUC", which = "best"))
  expect_identical(modres1[3, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[3]], "AUC", which = "best"))
  expect_identical(modres1[4, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[4]], "AUC", which = "best"))
  expect_equal(modres1[5, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[5]], "AUC", which = "best"))
  expect_identical(modres1[2, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[2]], "AUC", which = "all"))
  expect_false(identical(modres1[3, 3], caretEnsemble:::getMetricSD.train(ens.class$models[[3]],
                                                                          "AUC", which = "all")))
  expect_identical(modres2[1, 3], caretEnsemble:::getMetricSD.train(ens.reg$models[[1]], "RMSE", which = "best"))
  expect_identical(modres2[2, 3], caretEnsemble:::getMetricSD.train(ens.reg$models[[2]], "RMSE", which = "best"))
  expect_identical(modres2[1, 3], caretEnsemble:::getMetricSD.train(ens.reg$models[[1]], "RMSE", which = "all"))
  expect_false(identical(modres2[2, 3], caretEnsemble:::getMetricSD.train(ens.reg$models[[2]],
                                                                          "RMSE", which = "all")))
  modF <- caretEnsemble:::extractModFrame(ens.class)
  modF2 <- caretEnsemble:::extractModFrame(ens.reg)
  expect_true(ncol(modF) > ncol(ens.class$models[[2]]$trainingData))
  expect_true(ncol(modF2) > ncol(ens.reg$models[[1]]$trainingData))
  expect_true(nrow(modF) == nrow(ens.class$models[[2]]$trainingData))
  expect_true(nrow(modF2) == nrow(ens.reg$models[[1]]$trainingData))
})

context("Does prediction method work for regression")

test_that("We can ensemble models and handle missingness across predictors", {
  skip_on_cran()
  mseeds <- vector(mode = "list", length = 12)
  for(i in 1:11) mseeds[[i]] <- sample.int(1000, 1)
  mseeds[[12]] <- sample.int(1000, 1)
  myControl <- trainControl(method = "cv", number = 10, repeats = 1,
                           p = 0.75, savePrediction = TRUE,
                           returnResamp = "final",
                           returnData = TRUE, seeds = mseeds)

  trainC <- twoClassSim(n = 2000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
                        corrType = "AR1", corrValue = 0.6, mislabel = 0)

  testC <- twoClassSim(n = 1000, intercept = -9,  linearVars = 6, noiseVars = 4, corrVars = 2,
                       corrType = "AR1", corrValue = 0.6, mislabel = 0)
  MCAR.df <- function(df, p){
    MCARx <- function(x, p){
      z <- rbinom(length(x), 1, prob=p)
      x[z==1] <- NA
      return(x)
    }
    if(length(p) == 1){
      df <- apply(df, 2, MCARx, p)
    } else if(length(p) > 1) {
      df <- apply(df, 2, MCARx, sample(p, 1))
    }
    df <- as.data.frame(df)
    return(df)
  }
  set.seed(3256)
  trainC[, c(1:15)] <- MCAR.df(trainC[, c(1:15)], 0.15)
  testC[, c(1:15)] <- MCAR.df(testC[, c(1:15)], 0.05)
  set.seed(482)
  glm1 <- train(x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = "glm",
                trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm2 <- train(x = trainC[, c(1:15)], y = trainC[, "Corr2"], method = "glm",
                trControl = myControl, preProcess = "medianImpute", metric = "RMSE")
  set.seed(482)
  glm3 <- train(x = trainC[, c(2:9)], y = trainC[, "Corr2"], method = "glm",
                trControl = myControl, metric = "RMSE")
  set.seed(482)
  glm4 <- train(x = trainC[, c(1, 9:17)], y = trainC[, "Corr2"], method = "glm",
                trControl = myControl, metric = "RMSE")
  nestedList <- list(glm1, glm2, glm3, glm4)
  class(nestedList) <- "caretList"
  set.seed(482)
  ensNest <- caretEnsemble(nestedList, iter=2000)
  predobs <- caretEnsemble:::makePredObsMatrix(nestedList)
  greedOptRMSE(predobs$preds, predobs$obs)
  EnsNest <- caretEnsemble(nestedList, optFUN = greedOptRMSE, iter=100)
  pred.nest1 <- predict(ensNest, keepNA = TRUE, newdata=testC[, c(1:17)], se = TRUE)
  pred.nest1a <- predict(ensNest, newdata = testC[, c(1:17)], se=TRUE)
  pred.nest2 <- predict(ensNest, keepNA = FALSE, newdata = testC[, c(1:17)], se = TRUE)
  pred.nestTrain_a <- predict(ensNest, keepNA = FALSE, se =TRUE)
  expect_is(pred.nest1, "data.frame")
  expect_true(is.list(pred.nest2))
  expect_is(pred.nest1a, "data.frame")
  expect_is(pred.nestTrain_a, "data.frame")
  expect_identical(names(pred.nest1), c("pred", "se"))
  expect_identical(names(pred.nest2), c("pred", "se"))
  expect_identical(names(pred.nest2), names(pred.nest1))
  expect_identical(pred.nest1, pred.nest1a)
  expect_true(length(pred.nest1)==2)
})

#Reg tests
test_that("Prediction options are respected in regression and classification", {
  skip_on_cran()
  load(system.file("testdata/models.reg.rda", package="caretEnsemble", mustWork=TRUE))
  ens.reg <- caretEnsemble(models.reg, iter=1000)
  tests <- expand.grid(keepNA=0:1, se=0:1, return_weights=0:1)
  tests <- data.frame(lapply(tests, as.logical))
  for(i in 1:nrow(tests)){
    p <- predict(
      ens.reg,
      keepNA=tests[i,"keepNA"],
      se=tests[i,"se"],
      return_weights=tests[i,"return_weights"]
    )

    if(tests[i,"return_weights"]){
      expect_is(p, "list")
      preds <- p$preds
    } else{
      preds <- p
    }

    if(tests[i,"se"]){
      expect_is(preds, "data.frame")
    } else{
      expect_is(preds, "numeric")
    }
  }

  #Class tests
  load(system.file("testdata/models.class.rda", package="caretEnsemble", mustWork=TRUE))
  ens.class <- caretEnsemble(models.class, iter=1000)
  tests <- expand.grid(keepNA=0:1, se=0:1, return_weights=0:1)
  tests <- data.frame(lapply(tests, as.logical))
  for(i in 1:nrow(tests)){
    p <- predict(
      ens.class,
      keepNA=tests[i,"keepNA"],
      se=tests[i,"se"],
      return_weights=tests[i,"return_weights"]
    )

    if(tests[i,"return_weights"]){
      expect_is(p, "list")
      preds <- p$preds
    } else{
      preds <- p
    }

    if(tests[i,"se"]){
      expect_is(preds, "data.frame")
    } else{
      expect_is(preds, "numeric")
    }
  }
})
