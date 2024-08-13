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

reg_models <- sort(unique(all_models[which(forReg), ][["model"]]))
bin_models <- sort(unique(all_models[which(probModel), ][["model"]]))

bin_models <- setdiff(bin_models, "gaussprLinear") # Unbelievably slow.  100 points, 2 columns, 50 hours lol

#################################################################
# Class/Reg models
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
models_reg <- caretList(X, y, methodList = reg_models, tuneLength = 1L, continue_on_fail = TRUE)
models_reg <- trim_models(models_reg)

# Problem models: None!
models_bin <- caretList(X, y_bin, methodList = bin_models, tuneLength = 1L, continue_on_fail = TRUE)
models_bin <- trim_models(models_bin)

#################################################################
# Save data
#################################################################
all_models <- c(models_reg, models_bin)

usethis::use_data(
  all_models,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz",
  version = 3,
  ascii = FALSE
)