# This test takes a few minutes and needs to install and load a lot of packages
# I don't want to make it a dependency for the package or even for PR tests
# But I do want to run this every release to make sure that the models
# we can run predict correctly.

devtools::load_all()

very_quiet <- function(expr) {
  testthat::expect_output(suppressWarnings(suppressMessages(expr)))
}

#################################################################
# Setup data
#################################################################
set.seed(42L)
nrows <- 10L
ncols <- 2L

X <- matrix(stats::rnorm(nrows * ncols), ncol = ncols)
colnames(X) <- paste0("X", 1L:ncols)

y <- X[, 1L] + X[, 2L] + stats::rnorm(nrows) / 10.0
y_bin <- factor(ifelse(y > median(y), "yes", "no"))

all_models <- data.table::data.table(caret::modelLookup())
all_models <- unique(all_models[, c("model", "forReg", "probModel")])

java_models <- c(
  "gbm_h2o",
  "glmnet_h2o",
  "bartMachine",
  "M5",
  "M5Rules",
  "J48",
  "JRip",
  "LMT",
  "PART",
  "OneR",
  "evtree"
)

#################################################################
# Reg
#################################################################

# From https://github.com/zachmayer/caretEnsemble/issues/324
# Problem models:
# bam - array
# blackboost - matrix, array
# dnn - matrix, array
# earth - matrix, array
# gam - array
# gamboost - matrix, array
# glmboost - matrix, array
# pcaNNet - matrix, array
# rvmLinear - matrix, array
# rvmRadial - matrix, array
# spls - matrix, array
# xyf - matrix, array
reg_models <- sort(unique(all_models[which(forReg), ][["model"]]))
reg_models <- setdiff(reg_models, c( # Can't install or too slow
  "elm", "extraTrees", "foba", "logicBag", "mlpSGD", "mxnet",
  "mxnetAdam", "nodeHarvest", "relaxo",
  java_models
))

#################################################################
# Class
#################################################################

# Problem models: None!
bin_models <- sort(unique(all_models[which(probModel), ][["model"]]))
bin_models <- setdiff(bin_models, c( # Can't install or too slow
  "gaussprLinear", "adaboost", "amdai", "chaid", "extraTrees",
  "gpls", "logicBag", "mlpSGD", "mxnet", "mxnetAdam", "nodeHarvest",
  "ORFlog", "ORFpls", "ORFridge", "ORFsvm", "rrlda", "vbmpRadial",
  java_models
))

#################################################################
# Tests
#################################################################

testthat::test_that("Most caret models can predict", {
  # Fit the big caret lists
  models_reg <- very_quiet(caretList(X, y, methodList = reg_models, tuneLength = 1L, continue_on_fail = TRUE))
  models_bin <- very_quiet(caretList(X, y_bin, methodList = bin_models, tuneLength = 1L, continue_on_fail = TRUE))
  all_models <- c(models_reg, models_bin)
  testthat::expect_gt(length(all_models), 200L) # About 100 each of class/reg

  # Make sure we can predict
  pred <- very_quiet(predict(all_models, head(X, 5L)))
  testthat::expect_identical(nrow(pred), 5L)
  testthat::expect_identical(ncol(pred), length(all_models))
  testthat::expect_true(all(unlist(lapply(pred, is.finite))))

  # Make sure we can stacked predict
  # Some of these stupid models predict Infs lol, so whatever.
  # I guess beware of what models you ensemble.
  # The bagEarth models are bad, as is rvmPoly and some others.
  # These are stacked preds btw, so probably it indicates a fit failure
  # on one fold. Many ensemble models can handle Nans, but we'll see.
  pred_stack <- suppressWarnings(suppressMessages(predict(all_models)))
  testthat::expect_identical(nrow(pred_stack), nrow(X))
  testthat::expect_identical(ncol(pred_stack), length(all_models))
})
