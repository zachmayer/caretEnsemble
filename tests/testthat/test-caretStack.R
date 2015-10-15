
context("Does stacking and prediction work?")
library(caret)
library(randomForest)

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

test_that("We can stack regression models", {
  set.seed(96367)
  ens.reg <- caretStack(models.reg, method="lm", preProcess="pca",
                        trControl=trainControl(number=2, allowParallel=FALSE))
  expect_that(ens.reg, is_a("caretStack"))
  expect_is(summary(ens.reg), "summary.lm")
  sink <- capture.output(print(ens.reg))
  pred.reg <- predict(ens.reg, newdata = X.reg)
  expect_true(is.numeric(pred.reg))
  expect_true(length(pred.reg)==150)
})

test_that("We can stack classification models", {
  set.seed(42)
  ens.class <- caretStack(models.class, method="glm",
                          trControl=trainControl(number=2, allowParallel=FALSE))
  expect_that(ens.class, is_a("caretStack"))
  expect_is(summary(ens.class), "summary.glm")
  sink <- capture.output(print(ens.class))
  pred.class <- predict(ens.class, X.class, type="prob")
  expect_true(is.numeric(pred.class))
  expect_true(length(pred.class)==150)
  raw.class <- predict(ens.class, X.class, type="raw")
  expect_true(is.factor(raw.class))
  expect_true(length(raw.class)==150)
})

test_that("caretStack plots", {
  skip_if_not_installed("gbm")
  test_plot_file <- "caretEnsemble_test_plots.png"
  ens.reg <- caretStack(
    models.reg, method="gbm", tuneLength=2, verbose=FALSE,
    trControl=trainControl(number=2, allowParallel=FALSE))
  png(filename = test_plot_file)
  plot(ens.reg)
  dotplot(ens.reg, metric="RMSE")
  dev.off()
  unlink(test_plot_file)
})

context("Prediction errors for caretStack work as expected")

test_that("Failure to calculate se occurs gracefully", {
  ens.class <- caretStack(models.class, method="glm",
                          trControl=trainControl(number=2, allowParallel=FALSE))
  expect_message(predict(ens.class, X.class, type="raw", se = TRUE))
  expect_is(predict(ens.class, X.class, type="raw"), "factor")
  expect_is(predict(ens.class, X.class, type="raw", se=TRUE), "factor")
  expect_identical(predict(ens.class, X.class, type="raw", se=TRUE),
                   predict(ens.class, X.class, type="raw"))
  ens.reg <- caretStack(models.reg, method="lm", preProcess="pca",
                        trControl=trainControl(number=2, allowParallel=FALSE))
  expect_message(predict(ens.reg, X.reg, se=TRUE))
  expect_is(predict(ens.reg, X.reg, se=TRUE), "numeric")
  expect_identical(predict(ens.reg, X.reg, se=TRUE), predict(ens.reg, X.reg))
  #
  expect_is(predict(ens.class, X.class, type="prob", se=TRUE), "data.frame")
  expect_is(predict(ens.class, X.class, type="prob", se=TRUE, return_weights=TRUE),
            "data.frame")
  expect_identical(colnames(predict(ens.class, X.class, type="prob", se=TRUE)),
                   c("fit", "lwr", "upr"))
  expect_false(identical(predict(ens.class, X.class, type="raw", return_weights=TRUE),
                         predict(ens.class, X.class, type="raw", return_weights=FALSE)))
  expect_false(identical(predict(ens.class, X.class, type="prob", se = TRUE, level=0.8),
                         predict(ens.class, X.class, type="prob", se=TRUE, return_weights=FALSE)))
  expect_true(identical(predict(ens.class, X.class, type="prob", level=0.8),
                        predict(ens.class, X.class, type="prob", return_weights=FALSE)))
})
