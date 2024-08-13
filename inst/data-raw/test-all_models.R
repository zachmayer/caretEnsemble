# This test takes a few minutes and needs to install and load a lot of packages
# I don't want to make it a dependency for the package or even for PR tests
# But I do want to run this every release to make sure that the models
# we can run predict correctly.

devtools::load_all()

#################################################################
# Setup data
#################################################################
set.seed(42L)
nrows <- 25L
ncols <- 2L

X <- matrix(stats::rnorm(nrows * ncols), ncol = ncols)
colnames(X) <- paste0("X", 1L:ncols)

y <- X[, 1L] + X[, 2L] + stats::rnorm(nrows) / 10.0
y_bin <- factor(ifelse(y > median(y), "yes", "no"))

all_models <- data.table::data.table(caret::modelLookup())
all_models <- unique(all_models[, c("model", "forReg", "probModel")])

reg_models <- sort(unique(all_models[forReg, ][["model"]]))
bin_models <- sort(unique(all_models[probModel, ][["model"]]))

bin_models <- setdiff(bin_models, "gaussprLinear") # Unbelievably slow.  100 points, 2 columns, 50 hours lol

#################################################################
# Test fit AND predict
#################################################################

# We skip these by default, as you need to manually babysit them and install packages
# and for many models, the install fails, so you need to babysit every time.

# https://github.com/topepo/caret/blob/master/pkg/caret/R/trim.R
trim_models <- function(x) {
  for (n in names(x)) {
    removals <- c(
      "results", "pred", "bestTune", "call", "dots",
      "metric", "trainingData", "resample", "resampledCM",
      "perfNames", "maxmimize", "times"
    )
    for (i in removals) {
      if (i %in% names(x[[n]])) {
        x[[n]][[i]] <- NULL
      }
    }
    c_removals <- c(
      "method", "number", "repeats", "p", "initialWindow",
      "horizon", "fixedWindow", "verboseIter", "returnData",
      "returnResamp", "savePredictions", "summaryFunction",
      "selectionFunction", "index", "indexOut", "indexFinal",
      "timingSamps", "trim", "yLimits"
    )
    for (i in c_removals) {
      if (i %in% names(x[[n]]$control)) x[[n]]$control[i] <- NULL
    }

    if (!is.null(x[[n]]$modelInfo$trim)) {
      x[[n]]$finalModel <- x[[n]]$modelInfo$trim(x[[n]]$finalModel)
    }
  }
  x
}

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
testthat::test_that("All reg models work", {
  testthat::skip()
  models_reg <- caretList(X, y, methodList = reg_models, tuneLength = 1L, continue_on_fail = TRUE)
  models_reg <- trim_models(models_reg)
  saveRDS(models_reg, file.path("inst", "all_caret_reg_models.rds"))
  testthat::expect_gt(length(models_reg), 100L) # Should be about 109
  testthat::expect_gt(length(models_reg) / length(reg_models), 0.75) # 75% should work
  print(summary(models_reg)$results[order(value + sd), ])
  pred <- predict(models_reg, X)
  testthat::expect_identical(nrow(pred), nrow(X))
  testthat::expect_identical(ncol(pred), length(models_reg))
  testthat::expect_true(all(unlist(lapply(pred, is.finite))))
})

# Problem models: None!
testthat::test_that("All reg models work", {
  testthat::skip()
  models_bin <- caretList(X, y_bin, methodList = bin_models, tuneLength = 1L, continue_on_fail = TRUE)
  models_bin <- trim_models(models_bin)
  saveRDS(models_bin, file.path("inst", "all_caret_bin_models.rds"), compress = "xz")
  testthat::expect_gt(length(models_bin), 100L) # Should be about 109
  testthat::expect_gt(length(models_bin) / length(bin_models), 0.65) # 65% should work
  print(summary(models_bin)$results[order(-value + sd), ])
  pred <- predict(models_bin, X)
  testthat::expect_identical(nrow(pred), nrow(X))
  testthat::expect_identical(ncol(pred), length(models))
  testthat::expect_true(all(unlist(lapply(pred, is.finite))))
})

#################################################################
# Test predict only
#################################################################
# Even these tests require a large number of installed packages
# So we just run them manually, locally befgore each release
# you can use install.packages libs to quickly install all these packages

models_reg <- readRDS(file.path("inst", "all_caret_reg_models.rds"))
models_bin <- readRDS(file.path("inst", "all_caret_bin_models.rds"))
all_models <- c(models_reg, models_bin)

libs <- lapply(all_models, function(x) {
  x$modelInfo$library
})
libs <- sort(unique(unlist(libs)))

testthat::test_that("All libraries are installed", {
  is_installed <- vapply(libs, function(x) {
    suppressWarnings(suppressMessages(requireNamespace(x, quietly = TRUE)))
  }, logical(1L))
  if (!all(is_installed)) {
    stop("Lib not installed: ", libs[!is_installed], call. = FALSE)
  }
  testthat::expect_true(all(is_installed))
})

testthat::test_that("Most caret models can predict", {
  for (lib in libs) {
    # Caret needs the libs loaded.
    require(lib, character.only = TRUE) # nolint undesired_function_linter
  }
  testthat::expect_gt(length(all_models), 200L) # About 100 each of class/reg
  pred <- suppressWarnings(suppressMessages(predict(all_models, X)))
  testthat::expect_identical(nrow(pred), nrow(X))
  testthat::expect_identical(ncol(pred), length(all_models))
  testthat::expect_true(all(unlist(lapply(pred, is.finite))))
})
